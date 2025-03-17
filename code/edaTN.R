library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(tidyr)
library(car)
library(deltadata)
library(patchwork)
library(sf)

data <- list()

options(scipen = 99999)
theme_set(theme_bw(base_size = 22))

# Read in the files -------------------------------------------------------

# --- DS health data and meatadata ---
data$health <- read_excel("Data/Master_ordered_srdwsc_zoop.xlsx", sheet = "Master_ordered", 
                          guess_max = 1048576) # Value is the max number of rows in Excel
# collection_date must be a date
data$metadata <- read_excel("Data/Master_ordered_srdwsc_zoop.xlsx", sheet = "METADATA")
# Range of dataset is 08-23-2011 to 08-20-2021

# --- EDSM Survey data ---
data$edsmEdi <- getEDI("https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=415", 
                       files = c("EDSM_20mm.csv", "EDSM_KDTR.csv"))
# --- Spring Kodiak Trawl ---
data$skt <- bridgeAccess("https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SKT.zip",
                         c("tblCatch", "tblOrganismCodes", "tblSample", "tblFishInfo", "MSysRelationships")) %>% 
  {
    schema <- .$MSysRelationships
    df <- schemaJoin(schema, .) %>% 
      mutate(SampleDate = as.Date(SampleDate))
    
    # For SKT:
    # 2015-01-14, station 719 has two tows, only first has data. Will replicate to second tow as well
    # 2015-02-10, station 508 has three tows, replicating tows 2 and 3
    # 2015-03-11, station 719 has 2 twos, replicating to tow 2
    # All changes to data$skt
    replicatedTows <- df %>% 
      filter(SampleDate == as.Date("2015-01-14") & Station == 719 |
             SampleDate == as.Date("2015-02-10") & Station == 508 |
             SampleDate == as.Date("2015-03-11") & Station == 719) %>% 
      group_by(SampleDate, Station) %>% 
      fill(Secchi, ConductivityTop, WaterTemperature, .direction = "down")

    anti_join(
      df, replicatedTows,
      by = c("SampleDate", "Station")
    ) %>% 
      bind_rows(replicatedTows)
  }


# --- Summer Townet ---
stnFilePath <- file.path(tempdir(), "STN_Data1959-2023.accdb")
download.file("https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/TNS%20MS%20Access%20Data/TNS%20data/STN_Data1959-2023.accdb", 
              destfile = stnFilePath, 
              mode = "wb")

data$stn <- bridgeAccess(stnFilePath,
                         c("Length", "Sample", "TowEffort", "Catch", "MSysRelationships")) %>% 
  {
    schema <- .$MSysRelationships
    schemaJoin(schema, .)
  }
# --- Fall Midwater Trawl ---
data$fmwt <- bridgeAccess("https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/FMWT%20Data/MWT_data.zip",
                          c("Catch", "Length", "Sample", "MSysRelationships")) %>% 
  {
    schema <- .$MSysRelationships
    schemaJoin(schema, .)
  }

# --- Dayflow ---
data$dayflow <- read.csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/21c377fe-53b8-4bd6-9e1f-2025221be095/download/dayflow-results-1997-2023.csv") %>% 
  transmute(Date = as.Date(Date, format = "%m/%d/%Y"),
            SAC, SJR, OUT, X2)

# Document and Expand Metadata --------------------------------------------

# # --- Available vs described columns ---
# # There's a row in the medata that outlines the classes of zooplankton from column 3+
# # Removing this for now
# metadataNew <- data$metadata %>%
#   transmute(
#     variable = `BASIC INFO`,
#     decriptionExisting = `...2`,
#     presentInMetadata = T
#   ) %>%
#   full_join(
#     data.frame(
#       variable = names(health),
#       presentInData = T
#     ),
#     by = "variable"
#   ) %>%
#   mutate(
#     presentInBoth = ifelse(presentInMetadata & presentInData, T, F)
#   )
# 
# # Start present in metadata but missing in health
# metadataNew %>%
#   filter(is.na(presentInData) & presentInMetadata) %>%
#   View()
# 
# # Present in health but missing in metadata
# metadataNew %>%
#   filter(is.na(presentInMetadata) & presentInData) %>%
#   pull(variable) %>%
#   {
#     select(health, all_of(.))
#   } %>%
#   View()
# 
# # Present in both
# metadataNew %>%
#   filter(presentInData & presentInMetadata) %>%
#   View()
# 
# data$metadataNew <- read_xlsx("metadata.xlsx")
# 
# # Create full metadata combining existing with new
# data$metadataFinal <- data$metadata %>%
#   transmute(variableJoin = `BASIC INFO`,
#             descriptionExisting = `...2`,
#             presentInMetadata = T) %>%
#   filter(!is.na(variableJoin)) %>%
#   full_join(
#     data$metadataNew %>%
#       # If a column in the data has metadata but is just named differently from the value in the metadata file,
#       # use the metadata name to join in the respective metadata
#       mutate(useMetadataName = ifelse(!is.na(presentInMetadata) & !is.na(presentInData) & (presentInMetadata != presentInData),
#                                       T, F)) %>%
#       transmute(variable = presentInData,
#                 variableJoin = ifelse(useMetadataName, presentInMetadata, variable),
#                 notes,
#                 description,
#                 descriptionExisting,
#                 type),
#     by = c("variableJoin", "descriptionExisting")
#   ) %>%
#   mutate(inData = ifelse(variable %in% names(data$health), T, F), .after = variable) %>%
#   filter(inData) %>%
#   transmute(
#     variable, descriptionExisting, description, notes, inData, type
#   ) %>%
#   # Total NAs
#   left_join(
#     colSums(is.na(data$health)) %>%
#       data.frame(variable = names(.),
#                  numberNas = .,
#                  numberNasProportion = ./nrow(data$health),
#                  row.names = NULL),
#     by = "variable"
#   ) %>%
#   # Mean yearly NAs
#   left_join(
#     data$health %>%
#       group_by(year = year(collection_date)) %>%
#       summarise(across(everything(), ~sum(is.na(.))/nrow(data$health),
#                        .names = "{.col}")) %>%
#       pivot_longer(
#         cols = -year,
#         names_to = "variable",
#         values_to = "naCount"
#       ) %>%
#       filter(!is.na(year)) %>%
#       group_by(variable) %>%
#       summarise(naCountYearlyMean = mean(naCount)),
#     by = "variable"
#   ) %>%
#   as.data.frame()
# 
# # --- Visualize the metadata ---
# data$metadataFinal %>%
#   count(type) %>%
#   ggplot(aes(reorder(type, -n), n)) +
#   geom_col() +
#   geom_text(aes(label = n), vjust = -1, size = 5) +
#   labs(y = "NA Counts", x = "Variable Usefulness")
# # 106 variables to ignore, about 1/3 of the variable
# 
# # Of the remaining variables, how bad are the NAs
# data$metadataFinal %>%
#   filter(type != "ignore") %>%
#   distinct(variable, numberNasProportion) %>%
#   ggplot(aes(reorder(variable, numberNasProportion), numberNasProportion)) +
#   geom_col()
# # Need to now look through and see which ones are most useful using this updated metadata
# 
# write.csv(data$metadataFinal %>%
#             filter(type != "ignore") %>%
#             arrange(numberNas, variable),
#           "consideredNas.csv",
#           row.names = F)
# # Seems like NAs proportions less than 0.215372907 and 0.360350076 are my cutoffs
# 
# write.csv(data$metadataFinal %>%
#             filter(numberNasProportion <= 0.360350076 + .Machine$double.eps) %>%
#             arrange(numberNas, variable),
#           "consideringThreshold.csv",
#           row.names = F)
# 
# read.csv("consideredNas.csv", na.strings = "") %>%
#   filter(!is.na(typeAfterNas)) %>%
#   arrange(factor(typeAfterNas, levels = c("response", "engineer", "predictor"))) %>%
#   select(-type) %>%
#   write.csv("needMetadata.csv",
#             row.names = F)

# Final data columns
# This is to be ran AFTER the manual steps in lines 18-146
data$metadataFinal <- read.csv("needMetadata.csv")
potential <- list()
potential$response <- data$metadataFinal %>% 
  filter(
    typeAfterNas == "response"
  ) %>% 
  pull(variable)

potential$predictor <- data$metadataFinal %>% 
  filter(
    typeAfterNas != "response"
  ) %>% 
  pull(variable)

# --- Data distribution ---

# Response variables first
data$health %>% 
  transmute(across(all_of(potential$response), ~ as.numeric(.x))) %>% 
  na.omit() %>% 
  cor(method = "pearson") %>% 
  corrplot(method = "color",
           type = "upper",
           order = "hclust",
           addCoef.col = "black",
           tl.col = "black",
           tl.srt = 45,
           diag = F,
           title = "Pearson Correlation Matrix of Response Variables",
           mar=c(0,0,2,0))

# gonad_weight well correlated with gsi, likely gonad_weight / total_weight?
# liver_weight well correlated with hsi, hsi = liver_weight / total_weight?
# cf well correlated with cf_no_gonad

# Will remove:
# gill_weight and muscle_weight, per recommendation by Bruce
  # gill_weight is only HALF a gill; 
  # muscle weight is amount of tissue that went into the biochemistry assay

# Possible response:
# total_weight
# liver_weight
# brain_weight
# gonad_weight
# These are individual metrics. Likely better to the engineered metrics:
# cf: no
# cf_no_gonad
# hsi
# gsi: no, not as interested in reproductive growth
# Given cf_no_gonad is likely a more accurate representation of GROWTH than cf, will consider that over cf
  # Reasoning: likely plasticity in somatic vs reproductive growth depending on conditions, confounding if gonad weight is included

# cf_no_gonad has very low correlation to all other response variables, vs hsi which has SOME

# Looking at spearman just in case
par(mfrow = c(1, 2))

data$health %>% 
  transmute(across(all_of(potential$response), ~ as.numeric(.x))) %>% 
  na.omit() %>% 
  cor(method = "pearson") %>% 
  corrplot(method = "color",
           type = "upper",
           order = "original",
           addCoef.col = "black",
           tl.col = "black",
           tl.srt = 45,
           diag = F,
           title = "Pearson Correlation Matrix of Response Variables",
           mar=c(0,0,2,0))

data$health %>% 
  transmute(across(all_of(potential$response), ~ as.numeric(.x))) %>% 
  na.omit() %>% 
  cor(method = "spearman") %>% 
  corrplot(method = "color",
           type = "upper",
           order = "original",
           addCoef.col = "black",
           tl.col = "black",
           tl.srt = 45,
           diag = F,
           title = "Spearman Correlation Matrix of Response Variables",
           mar=c(0,0,2,0))

par(mfrow = c(1, 1))

# Pairs plot
data$health %>% 
  transmute(across(all_of(potential$response), ~ as.numeric(.x))) %>% 
  na.omit() %>% 
  pairs(main = "Scatterplot Matrix of Response Variables",
        pch = 20,  
        cex = 0.5, 
        col = "steelblue") 
# Obvious outliers in:
# gill_weight
# brain_weight
# Thresholds are:
plot(data$health %>% 
       mutate(gill_weight = as.numeric(gill_weight)) %>% 
       filter(gill_weight < 0.6) %>%
       pull(gill_weight))
plot(data$health %>% 
       mutate(brain_weight = as.numeric(brain_weight)) %>% 
       filter(brain_weight < 0.03) %>%
       pull(brain_weight))

data$health %>% 
  transmute(across(all_of(potential$response), ~ as.numeric(.x))) %>% 
  filter(gill_weight < 0.6, brain_weight < 0.03) %>% 
  na.omit() %>% 
  pairs(main = "Scatterplot Matrix of Response Variables",
        pch = 20,  
        cex = 0.5, 
        col = "steelblue") 

data$health %>% 
  transmute(across(all_of(potential$response), ~ as.numeric(.x))) %>% 
  filter(gill_weight < 0.6, brain_weight < 0.03) %>% 
  na.omit() %>% 
  pairsCorrelation(histogram = TRUE,
                   pch = 20,
                   method = "pearson",
                   main = "pearson Correlation",
                   cex.labels = 2)

# For cf_no_gonad, not really correlated against anything at all (pearson or spearman doesn't really matter)
  # cf does have slightly better correlations
# For hsi, more correlation, so perhaps easier to fit a model to this
# --- Exploring the distributions ---

# First with histogram
hist(data$health$cf, col = "light gray", probability = TRUE,
     axes = T, main = "cf", breaks = "FD")
hist(data$health$cf_no_gonad, col = "light gray", probability = TRUE,
     axes = T, main = "cf_no_gonad", breaks = "FD")
hist(data$health$hsi, col = "light gray", probability = TRUE,
     axes = T, main = "hsi", breaks = "FD")
# cf, cf_no_gonad look normal
# hsi is skewed. can try log/sqrt
hist(log(data$health$hsi), col = "light gray", probability = TRUE,
     axes = T, main = "log hsi", breaks = "FD")
# Weird bimodal?
hist(sqrt(data$health$hsi), col = "light gray", probability = TRUE,
     axes = T, main = "sqrt hsi", breaks = "FD")
# sqrt does a better job at making it more normal

# Then with qqplot
qqPlot(data$health$cf)
qqPlot(data$health$cf_no_gonad)
# Bit more right skewed than cf
# Both show heavy tails though

qqPlot(data$health$hsi)
# Very right skewed
qqPlot(sqrt(data$health$hsi))
# Still right skewed
qqPlot(log(data$health$hsi))
# Not any better
data$health %>% 
  filter(sex == 0) %>% 
  pull(hsi) %>% 
  sqrt() %>% 
  qqPlot
data$health %>% 
  filter(sex == 1) %>% 
  pull(hsi) %>% 
  sqrt() %>% 
  qqPlot
# Not perfectly normal either
# For now, will likely do a sqrt transformation; will need to verify the residuals

# Exploring maybe bimodal?
# Create the histogram with ggplot2
ggplot(data$health %>% 
         mutate(hsi = log(hsi),
                sex = factor(ifelse(sex == 0, "female", "male")),
                year = year(collection_date)) %>% 
         filter(!is.na(hsi), !is.na(sex), !is.na(year)), 
       aes(x = hsi, fill = sex, group = sex)) +
  geom_histogram(aes(y = after_stat(density)), 
                 # fill = "light gray", 
                 color = "black"
                 ) +
  labs(title = "log hsi", 
       x = "log(hsi)", 
       y = "Density") +
  facet_wrap(~year) 
# Appears to be caused by sex, but this effect is different from year to year
# year and sex likely will have to be in the model, but our sex data isn't as robust...

# Need to explore the sex data
data$health %>% 
  transmute(collection_date,
            across(contains("sex"), ~ ifelse(is.na(.x), T, F))) %>% 
  pivot_longer(cols = c(dissect_sex, histo_sex, sex),
               names_to = "sexColumns",
               values_to = "value") %>%
  # filter(is.na(value)) %>% 
  ggplot(aes(x = collection_date, y = sexColumns, color = sexColumns, shape = sexColumns)) +
  geom_point() +
  scale_shape_manual(values = c(16, 15, 17)) +
  labs(title = "NA Occurrences Across Sex Columns",
       x = "Collection Date",
       y = "Sex Column") +
  facet_wrap(~value) +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")

# Sex data not taken across all available years
# Exploring NAs of sex data

data$health %>% 
  transmute(collection_date,
            dissect_sex = ifelse(!is.na(dissect_sex), "dissectSex", NA_character_),
            histo_sex = ifelse(!is.na(histo_sex), "histoSex", NA_character_),
            sex = ifelse(!is.na(sex), "sex", NA_character_),
            alignment = paste(dissect_sex, histo_sex, sex, sep = "."),
            alignment = factor(alignment, levels = c(
              "dissectSex.histoSex.sex", 
              "dissectSex.histoSex.NA", "dissectSex.NA.sex", "NA.histoSex.sex",
              "dissectSex.NA.NA", "NA.histoSex.NA", "NA.NA.sex",
              "NA.NA.NA"
            ))) %>% 
  {
    frequency <- count(., alignment) %>%
      complete(alignment = factor(levels(.$alignment), levels = levels(.$alignment)), 
               fill = list(n = 0))
    
    labels <- paste0(frequency$alignment, " (", frequency$n, ")")
    names(labels) <- frequency$alignment
    
    ggplot(data = ., 
           aes(x = collection_date, 
               y = alignment)) +
      geom_point() +
      labs(title = "NA Occurrences Across Sex Columns",
           x = "Collection Date",
           y = "Sex Column") +
      scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
      scale_y_discrete(drop = FALSE, labels = labels)
  }

# Ok...what's the NA distribution here
data$naRange <- data$health %>% 
  # Convert collection_date to proper datetime if it's not already
  mutate(collection_date = as.Date(collection_date),
         across(-collection_date, ~ is.na(.x))) %>%
  # Gather all columns except collection_date
  pivot_longer(cols = -collection_date, 
               names_to = "variableName", 
               values_to = "value") %>%
  # Filter out NA values
  filter(value == F, !is.na(collection_date)) %>%
  # Group by variable name
  group_by(variableName) %>%
  # Summarize to get start and end dates
  summarize(
    startDate = min(collection_date),
    endDate = max(collection_date), 
    .groups = "drop"
  ) %>% 
  mutate(dayDifference = endDate - startDate) %>% 
  pivot_longer(
    c(endDate, startDate), names_to = "range", values_to = "date"
  )

data$naRange %>%   
  ggplot(
    aes(date, reorder(variableName, dayDifference), color = range)
  ) +
  geom_point()

data$naRange %>% 
  pivot_wider(names_from = "range", values_from = date) %>% 
  filter(year(endDate) == 2017) %>% 
  pivot_longer(
    c(endDate, startDate), names_to = "range", values_to = "date"
  ) %>% 
  ggplot(
    aes(date, reorder(variableName, dayDifference), color = range)
  ) +
  geom_point()

# --- Exploring the predictors ---

# Exploring the predictors ------------------------------------------------

# --- Updating data with trawling data ---
dataJoined <- list()

# --- EDSM ---
joinedEdsm <- data$health %>% 
  filter(agency == "usfws" | survey_char == "edsm") %>% 
  select(agency, survey_char, collection_date, depth_bottom, fish_id, 
         fork_length, cf, cf_no_gonad, hsi,
         avg_temp, avg_turb) %>% 
  left_join(
    bind_rows(
      data$edsmEdi$EDSM_20mm.csv %>% 
        transmute(StationCode, 
                  SampleDate = as.Date(SampleDate), 
                  MethodCode,
                  Latitude = LatitudeStart, 
                  Longitude = LongitudeStart,
                  Secchi, BottomDepth, Tide,
                  WaterTemp = WaterTempTop, 
                  SpecificConductance = SpecificConductanceTop, 
                  TurbidityNTU = TurbidityTopNTU,
                  DO = DOTop,
                  CommonName, ForkLength, SpecialStudy, SpecialStudyID,
                  originalSpecialStudyID = SpecialStudyID, 
                  FishComments, Count),
      data$edsmEdi$EDSM_KDTR.csv %>% 
        transmute(StationCode, 
                  SampleDate = as.Date(SampleDate), 
                  MethodCode, BottomDepth, Tide,
                  Latitude, Longitude,
                  Secchi, WaterTemp, SpecificConductance, 
                  TurbidityNTU, DO,
                  CommonName, ForkLength, SpecialStudy, 
                  SpecialStudyID,
                  originalSpecialStudyID = SpecialStudyID, 
                  Count)
    ) %>% 
      separate_longer_delim(SpecialStudyID, delim = "/"),
    by = c("fish_id" = "SpecialStudyID")
  )

edsmDifference <- joinedEdsm %>% 
  mutate(
    dateDifferencePivot = as.numeric(as.Date(collection_date) - SampleDate),
    forklengthDifferencePivot = fork_length - ForkLength,
    waterTemperatureDifferencePivot = avg_temp - WaterTemp,
    turbidityDifferencePivot = avg_turb - TurbidityNTU,
    depthDifferencePivot = depth_bottom - BottomDepth,
  ) %>% 
  pivot_longer(contains("DifferencePivot"),
               names_to = "variables", values_to = "difference",
               names_pattern = "(.*)DifferencePivot") %>% 
  mutate(difference = round(difference, 1),
         differenceBinary = difference == 0)

edsmDifference %>% 
  count(variables, differenceBinary) %>% 
  arrange(differenceBinary, -n)
# Many of the FLs are different...? Going to go with EDSM's FLs
# 1 instance of turbidity different
edsmDifference %>% 
  filter(difference != 0, variables == "turbidity")
# Appears to be 1 decimal point different; likely transcription error. Going
# with edsm
# Two dates that are different
edsmDifference %>% 
  filter(difference != 0, variables == "date") %>% 
  View()
# A week's apart. One in suisun and th eother in the confluence; unlikely that
# this was done in the same day. Defaulting to EDSM's date. Also takes care of
# the three entries with missing dates

# Bottom depth was not recorded until later into the survey

dataJoined$edsm <- joinedEdsm %>% 
  transmute(
    agency, survey_char, 
    collection_date = SampleDate,
    fish_id, fork_length,
    cf, cf_no_gonad, hsi, 
    station = StationCode, 
    MethodCode, Latitude, Longitude, Tide,
    WaterTemperature = WaterTemp, 
    Secchi, SpecificConductance, TurbidityNTU, 
    DissolvedOxygen = DO,
    DepthBottom = BottomDepth
  )

# --- SKT ---
# Seems as though there are some mix up with the fish id tags. Most of the ids
# match up to the SKT's FishID1 and FishID2, but some values are also from
# the diet study (investigated with Vanessa Mora)

# As such, will do a tiered combination:
# 1. by fish ID, sample date, and station
# 2. by sample date and station only

joinedSkt <- data$health %>% 
  filter(survey_char == "skt") %>% 
  transmute(agency, survey_char, collection_date, depth_bottom, fish_id,
            year = year(collection_date),
            fork_length, cf, cf_no_gonad, hsi,
            station,
            dfw_tidecode, dfw_temp, dfw_spcond, 
            dfw_sal, dfw_turb) %>% 
  separate_wider_delim(cols = "fish_id", delim = "skt", names = c("fish_id1", "fish_id2"),
                       too_few = "align_end", cols_remove = F) %>% 
  left_join(
    data$skt %>% 
      transmute(collection_date = SampleDate,
                SampleDate = SampleDate,
                SurveyNumber, Station, 
                Secchi, ConductivityTop, WaterTemperature, NTU, DepthBottom,
                TideCode, 
                OrganismCode, Catch, FishID1, FishID2, ForkLength),
    by = c("fish_id2" = "FishID2", 
           "year" = "FishID1",
           "collection_date",
           "station" = "Station")
  )

# This will give a many-to-many relationship error
# using `slice_min()`will overcome this
remainingSkt <- joinedSkt %>% 
  filter(is.na(SampleDate)) %>% 
  select(-c(SurveyNumber, Secchi, ConductivityTop, WaterTemperature, NTU,
            DepthBottom, TideCode, OrganismCode, Catch, ForkLength,
            SampleDate)) %>% 
  mutate(healthIndex = row_number()) %>% 
  left_join(
    data$skt %>% 
      transmute(collection_date = SampleDate,
                SampleDate = SampleDate,
                SurveyNumber, Station, 
                across(c(Secchi, ConductivityTop, WaterTemperature, NTU, DepthBottom,
                         TideCode),
                       ~ round(.x, 1))) %>% 
      distinct() %>% 
      mutate(sktIndex = row_number()),
    by = c("collection_date",
           "station" = "Station")
  ) %>% 
  relocate(contains("Index")) %>% 
  mutate(
    tideCodeDifference = dfw_tidecode - TideCode,
    conductivityDifference = dfw_spcond - ConductivityTop,
    waterTemperatureDifference = dfw_temp - WaterTemperature,
    turbidityDifference = NTU - dfw_turb,
    depthDifference = depth_bottom - DepthBottom,
    totalDifference = rowSums(across(c(tideCodeDifference, conductivityDifference,  
                                       waterTemperatureDifference, turbidityDifference, depthDifference),
                                     ~ abs(.x)))
  ) %>% 
  group_by(healthIndex) %>% 
  # Keep only the row with minimum difference or NA if all are NA
  slice_min(totalDifference, with_ties = FALSE) %>%
  # # If all were NA, keep one row?
  # slice_head(n = 1) %>%
  ungroup() %>% 
  select(-c(tideCodeDifference, conductivityDifference, waterTemperatureDifference,
            turbidityDifference, depthDifference, totalDifference,
            sktIndex, healthIndex))

dataJoined$skt <- anti_join(joinedSkt,
                            remainingSkt,
                            by = c("fish_id")) %>% 
  bind_rows(
    remainingSkt
  )

sktDifference <- dataJoined$skt %>% 
  mutate(
    dateDifferencePivot = as.numeric(as.Date(collection_date) - SampleDate),
    tideCodeDifferencePivot = dfw_tidecode - TideCode,
    conductivityDifferencePivot = dfw_spcond - ConductivityTop,
    waterTemperatureDifferencePivot = dfw_temp - WaterTemperature,
    turbidityDifferencePivot = dfw_turb - NTU,
    depthDifferencePivot = depth_bottom - DepthBottom
  ) %>% 
  pivot_longer(contains("DifferencePivot"),
               names_to = "variables", values_to = "difference",
               names_pattern = "(.*)DifferencePivot") %>% 
  mutate(difference = round(difference, 1),
         differenceBinary = difference == 0)

sktDifference %>% 
  count(variables, differenceBinary) %>% 
  arrange(differenceBinary, -n)
# Matches for the most part; turbidity differences due to error in the FTP database (single vs double floating point)
# Investigate the difference in bottom depth though
sktDifference %>% 
  filter(variables == "depth", difference != 0) %>% 
  pull(fish_id) %>% 
  {
    affectedIds <- .
    data$skt %>% 
      filter(FishID2 %in% affectedIds)
  } %>% 
  View()

# This is due to TWO tows at 719 here, survey # 4 and 14
# The water quality values are all the same except for bottom depth. Bruce's dataset
# does not account for the second tow (of which some of these fish_ids are from)
# Going to keep depth from the CDFW survey
data$skt %>% 
  filter(as.Date(SampleDate) == as.Date("2012-04-04"),
         Station == 719) %>% 
  View()

# There are 7 sample IDs that have NAs for the water quality metrics
# These are all replicate tows in which water quality was only measured during the first
# pull. Will fill in these with values from the first tow

# Adding the lat/lon to SKT stations
dataJoined$skt <- dataJoined$skt %>% 
  left_join(
    bridgeAccess("https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SKT.zip", "StationsSKT")[[1]] %>% 
      transmute(
        station = Station,
        Latitude = decimalDegrees(paste(LatDeg, LatMin, LatSec), type = "dms"),
        Longitude = decimalDegrees(paste(LongDec, LongMin, LongSec), type = "dms", isLongitude = T)
      ),
    by = "station"
  ) %>% 
  transmute(
    agency, survey_char, 
    collection_date = SampleDate,
    fish_id, fork_length,
    cf, cf_no_gonad, hsi, 
    station, 
    MethodCode = "KDTR", 
    Latitude, Longitude,
    WaterTemperature = WaterTemperature, 
    Secchi, 
    SpecificConductance = ConductivityTop, 
    TurbidityNTU = NTU, 
    Tide = case_when(
      TideCode == 1 ~ "High Slack",
      TideCode == 2 ~ "Ebb",
      TideCode == 3 ~ "Low Slack",
      TideCode == 4 ~ "Flood"
    ),
    DepthBottom
  )

# --- STN ---
data$health %>% 
  filter(agency == "cdfw", survey_char == "stn",
         collection_date == as.Date("2011-08-23"),
         station == 716) %>% 
  View()

data$stn %>% 
  filter(SampleDate == as.Date("2011-08-23"), 
         StationCode == 716,
         OrganismCode == 3) %>% 
  View()

# Having difficulties trying to find the fish ID equivalent. Will simply
# bind by date and station

joinedStn <- data$health %>% 
  filter(agency == "cdfw", survey_char == "stn") %>% # 438 rows
  transmute(agency, survey_char, collection_date, depth_bottom, fish_id,
            year = year(collection_date),
            station,
            fork_length, cf, cf_no_gonad, hsi,
            dfw_tidecode, dfw_temp, dfw_spcond, 
            dfw_sal, dfw_turb) %>% 
  left_join(
    data$stn %>% 
      transmute(collection_date = SampleDate,
                SampleDate,
                station = StationCode,
                TemperatureTop, TemperatureBottom, Secchi, ConductivityTop,
                ConductivityBottom, TideCode, DepthBottom, TurbidityTop) %>% 
      distinct(),
    by = c("collection_date", "station")
  )


stnDifference <- joinedStn %>% 
  mutate(
    tideCodeDifferencePivot = dfw_tidecode - TideCode,
    conductivityDifferencePivot = dfw_spcond - ConductivityTop,
    waterTemperatureDifferencePivot = dfw_temp - TemperatureTop,
    turbidityDifferencePivot = dfw_turb - TurbidityTop,
    depthDifferencePivot = depth_bottom - DepthBottom
  ) %>% 
  pivot_longer(contains("DifferencePivot"),
               names_to = "variables", values_to = "difference",
               names_pattern = "(.*)DifferencePivot") %>% 
  mutate(difference = round(difference, 1),
         differenceBinary = difference == 0)

stnDifference %>% 
  count(variables, differenceBinary) %>% 
  arrange(differenceBinary, -n)
# Seems like 1 instance of major difference
stnDifference %>% 
  filter(difference != 0) %>% 
  distinct(collection_date, SampleDate, station) %>% 
  {
    dateStation <- .
    joinedStn %>% 
      filter(collection_date == dateStation$collection_date,
             station == dateStation$station)
  } %>% 
  View()
# Seems as though Bruce's dataset might be wrong here; historically, station 519
# has a depth range of median 10, mean 9, and max of 30, not 41
stnDifference %>% 
  filter(if_any(c("dfw_tidecode", "dfw_temp", "dfw_spcond", "dfw_sal", "dfw_turb"), ~!is.na(.x)),
         if_any(c("TemperatureTop", "TemperatureBottom", "Secchi", "ConductivityTop", 
                  "ConductivityBottom", "TideCode", "DepthBottom", "TurbidityTop"), ~is.na(.x))) %>% 
  distinct(collection_date, station)
# 3 stations, but those don't have data on Bruce's side as well. OK to proceed with using CDFW data

# Adding lat/lon to final
dataJoined$stn <- joinedStn %>% 
  left_join(
    bridgeAccess(stnFilePath, "luStation")[[1]] %>% 
      filter(Active) %>% 
      transmute(
        station = StationCodeSTN, 
        Latitude = decimalDegrees(paste(LatD, LatM, LatS), type = "dms"),
        Longitude = decimalDegrees(paste(LonD, LonM, LonS), type = "dms", isLongitude = T)
      ),
    by = "station"
  ) %>% 
  transmute(
    agency, survey_char, 
    collection_date = SampleDate,
    fish_id, fork_length,
    cf, cf_no_gonad, hsi, 
    station, 
    MethodCode = "MWT", 
    Latitude, Longitude,
    WaterTemperature = TemperatureTop, 
    Secchi, 
    SpecificConductance = ConductivityTop, 
    TurbidityNTU = TurbidityTop, 
    Tide = case_when(
      TideCode == 1 ~ "High Slack",
      TideCode == 2 ~ "Ebb",
      TideCode == 3 ~ "Low Slack",
      TideCode == 4 ~ "Flood"
    ),
    DepthBottom
  )

# --- FMWT ---
# Same issue with FMWT as STN. Don't know how the fish_id's are recorded in the database
# Going to just bind on date/station
joinedFmwt <- data$health %>% 
  filter(agency == "cdfw", survey_char == "fmwt") %>% # 379 rows
  transmute(agency, survey_char, collection_date, depth_bottom, fish_id,
            year = year(collection_date),
            station,
            fork_length, cf, cf_no_gonad, hsi,
            dfw_tidecode, dfw_temp, dfw_spcond, 
            dfw_sal, dfw_turb) %>% 
  left_join(
    data$fmwt %>% 
      transmute(collection_date = SampleDate,
                SampleDate,
                station = StationCode,
                WaterTemperature, Secchi, ConductivityTop,
                ConductivityBottom, TideCode, DepthBottom, Turbidity) %>% 
      distinct(),
    by = c("collection_date", "station")
  )

fmwtDifference <- joinedFmwt %>% 
  mutate(
    tideCodeDifferencePivot = dfw_tidecode - TideCode,
    conductivityDifferencePivot = dfw_spcond - ConductivityTop,
    waterTemperatureDifferencePivot = dfw_temp - WaterTemperature,
    turbidityDifferencePivot = dfw_turb - Turbidity,
    depthDifferencePivot = depth_bottom - DepthBottom
  ) %>% 
  pivot_longer(contains("DifferencePivot"),
               names_to = "variables", values_to = "difference",
               names_pattern = "(.*)DifferencePivot") %>% 
  mutate(difference = round(difference, 1),
         differenceBinary = difference == 0)

fmwtDifference %>% 
  count(variables, differenceBinary) %>% 
  arrange(differenceBinary, -n)
# 2 instances of differences
fmwtDifference %>% 
  filter(difference != 0) %>% 
  distinct(collection_date, SampleDate, station) %>% 
  {
    dateStation <- .
    joinedFmwt %>% 
      filter(collection_date == dateStation$collection_date,
             station == dateStation$station)
  } %>% 
  View()
# Can't tell which one is wrong; going to default to the FMWT
# 2014-12-17, station 719, only conductivity, turbidity, and water temperature are different


fmwtDifference %>% 
  filter(if_any(c("dfw_tidecode", "dfw_temp", "dfw_spcond", "dfw_sal", "dfw_turb"), ~!is.na(.x)),
         if_any(c("WaterTemperature", "Secchi", "ConductivityTop", 
                  "ConductivityBottom", "TideCode", "DepthBottom", "Turbidity"), ~is.na(.x))) %>% 
  distinct(collection_date, station)

dataJoined$fmwt <- joinedFmwt %>% 
  left_join(
    bridgeAccess("https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/FMWT%20Data/MWT_data.zip",
                 "StationsLookUp")[[1]] %>% 
      transmute(
        station = StationCode,
        Latitude = as.numeric(DD_Latitude),
        Longitude = as.numeric(DD_Longitude)
      ),
    by = "station"
  ) %>% 
  transmute(
    agency, survey_char, 
    collection_date = SampleDate,
    fish_id, fork_length,
    cf, cf_no_gonad, hsi, 
    station, 
    MethodCode = "MWT", 
    Latitude, Longitude,
    WaterTemperature, 
    Secchi, 
    SpecificConductance = ConductivityTop, 
    TurbidityNTU = Turbidity, 
    Tide = case_when(
      TideCode == 1 ~ "High Slack",
      TideCode == 2 ~ "Ebb",
      TideCode == 3 ~ "Low Slack",
      TideCode == 4 ~ "Flood"
    ),
    DepthBottom
  )

# Final dataset -----------------------------------------------------------

# Bind all datasets with survey data so far:
data$final <- bind_rows(dataJoined) %>% # 2628 rows, same as original
  arrange(collection_date) %>% 
  # Bind in the regional assignments, using EDSM subregions
  st_as_sf(coords = c("Longitude", "Latitude"), 
           crs = st_crs(4326)) %>% # Geographic crs first (raw lat/lon)
  st_transform(crs = 3310) %>% # Projected for calculations
  st_join(deltamapr::R_EDSM_Subregions_Mahardja_FLOAT %>% 
            st_transform(crs = 3310), join = st_intersects) %>% 
  st_drop_geometry() %>% 
  select(-c(SQM, Region)) %>% 
  # Add in dayflow data
  left_join(
    data$dayflow,
    by = c("collection_date" = "Date")
  )

# Checking for data completeness
missingEnvironmental <- data$final %>% 
  filter(is.na(WaterTemperature) | is.na(Secchi) | is.na(SpecificConductance))

# For SKT:
# 2015-01-14, station 719 has two tows, only first has data. Will replicate to second tow as well
# 2015-02-10, station 508 has three tows, replicating tows 2 and 3
# 2015-03-11, station 719 has 2 twos, replicating to tow 2
# All changes to data$skt
# For EDSM:

data$skt %>% 
  filter(SampleDate == as.Date("2015-03-11"),
         Station == 719) %>% 
  View()

bind_rows(select(data$edsmEdi$EDSM_20mm.csv, -c(StageCode, TagCode)),
          select(data$edsmEdi$EDSM_KDTR.csv, -c(StageCode, TagCode))) %>% 
  separate_longer_delim(SpecialStudyID, delim = "/") %>% 
  {
    df <- .
    semi_join(
      df,
      df %>% 
        filter(SpecialStudyID %in% pull(filter(missingEnvironmental, survey_char == "edsm"), fish_id)) %>% 
        distinct(SampleDate, StationCode),
      by = c("SampleDate", "StationCode")
    )
  } %>% 
  View()
# Don't really know why these don't have environmental data. Asked Claudia, awaiting answer

# I think this is a good stopping point for now. Will now EDA
# Currently have:
# cf, cf_no_gonad, hsi
# station, MethodCode, Tide, WaterTemperature, Secchi, 

# Dayflow variables: x2, outflow
# Spatial variables: region1, distance to tidal marsh/nontidal wetlands
# Water quality variables: avg_sal/avg_spcond, avg_turb, chl_a_avg, avg_temp,
data$health %>% 
  select(
    # logistic
    collection_date,
    # dayflow
    x2_actual, log_outflow,
    # spatial
    region1, tidal_marsh_km, nontidal_wetlands_km,
    # water quality
    avg_sal, avg_turb, avg_temp, dfw_sal, dfw_spcond, dfw_turb, dfw_temp, avg_spcond, 
    # food
    chl_a_avg, zoop_biomass_mg, sum_total_prey, other_gc, cladocerans_gc, other_calanoids_gc,
    as_gc, limnoithona_spp_gc, mysids_gc, other_cyclopoids_gc, pf_spp_gc, sd_gc
  )

# What are some other variables that might be useful but not included after the initial pass:
# sex: histo_sex, dissect_sex, sex
# liver_inf, gill_inf
# avg_ph
# avg_dopercent
# 



  

