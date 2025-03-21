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
stnFilePath <- file.path(tempdir(), "STN_Data1959-2024.accdb")
download.file("https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/TNS%20MS%20Access%20Data/TNS%20data/STN_Data1959-2024.accdb", 
              destfile = stnFilePath, 
              mode = "wb")

data$stn <- bridgeAccess(stnFilePath,
                         c("Length", "Sample", "TowEffort", "Catch",
                           "MSysRelationships")) %>% 
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
# This is to be ran AFTER the manual steps above that are commented out
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

# Exploring response ------------------------------------------------------
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

# Exploring NA distributions ----------------------------------------------
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

# --- Filling in sex ---
# Only get 7 extra entries in sex per plot above...From dissectSex
data$health <- data$health %>% 
  mutate(sex = coalesce(sex, dissect_sex, histo_sex))

# # Confirm:
# data$health %>% 
#   mutate(sexFilled = coalesce(sex, dissect_sex, histo_sex)) %>% # Change sex to sexFilled above to check
#   select(contains("sex")) %>% 
#   is.na() %>% 
#   colSums()

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

# What about when limited to just the summer and fall?

# Updating trawl data -----------------------------------------------------
dataJoined <- list()

# Updating EDSM -----------------------------------------------------------
joinedEdsm <- data$health %>% 
  filter(agency == "usfws" | survey_char == "edsm") %>% 
  select(agency, survey_char, collection_date, depth_bottom, fish_id, 
         fork_length, cf, cf_no_gonad, hsi,
         avg_temp, avg_turb, sex) %>% 
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
    fish_id, fork_length, sex,
    cf, cf_no_gonad, hsi, 
    station = StationCode, 
    MethodCode, Latitude, Longitude, Tide,
    WaterTemperature = WaterTemp, 
    Secchi, SpecificConductance, TurbidityNTU, 
    DissolvedOxygen = DO,
    DepthBottom = BottomDepth
  )

# Updating SKT ------------------------------------------------------------
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
            dfw_sal, dfw_turb, sex) %>% 
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
    fish_id, fork_length, sex,
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

# Updating STN ------------------------------------------------------------

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

# In speaking with Margaret Johnson (ES Lead of STN), SerialNumber from the
# LengthSupplement table is the key to identify fish that were sent to UCD

joinedStn <- data$health %>% 
  filter(agency == "cdfw", survey_char == "stn") %>% # 438 rows
  transmute(agency, survey_char, collection_date, depth_bottom, fish_id,
            year = year(collection_date),
            station,
            fork_length, cf, cf_no_gonad, hsi,
            dfw_tidecode, dfw_temp, dfw_spcond, 
            dfw_sal, dfw_turb, sex) %>% 
  # separate_wider_delim(cols = "fish_id", delim = "stn", names = c("fish_id1", "fish_id2"),
  #                      too_few = "align_end", cols_remove = F) %>% # Missing fishid's on the STN side. Skipping
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
    fish_id, fork_length, sex,
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

# Updating FMWT -----------------------------------------------------------
# Same issue with FMWT as STN. Don't know how the fish_id's are recorded in the database
# Going to just bind on date/station
joinedFmwt <- data$health %>% 
  filter(agency == "cdfw", survey_char == "fmwt") %>% # 379 rows
  transmute(agency, survey_char, collection_date, depth_bottom, fish_id,
            year = year(collection_date),
            station,
            fork_length, cf, cf_no_gonad, hsi, sex,
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
    fish_id, fork_length, sex,
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
  ) %>% 
  mutate(month = month(collection_date, label = T, abbr = T),
         season = factor(
           case_when(as.numeric(month) %in% c(1, 2, 12) ~ "Winter", # Winter survival
                     as.numeric(month) %in% c(3:5) ~ "Spring", # recruitment period
                     as.numeric(month) %in% 6:8 ~ "Summer", # summer survival
                     as.numeric(month) %in% 9:11 ~ "Fall"),
           levels = c("Spring", "Summer", "Fall", "Winter")
         ), # fall survival
         # Seasons based on Polansky et al. 2024, Table 1
         survey_char = factor(survey_char, levels = c("skt", "stn", "edsm", "fmwt")),
         sex = factor(ifelse(sex == 0, "Female", "Male")),
         # Secchi data differs between the surveys
         Secchi = ifelse(survey_char %in% c("skt", "stn"),
                         Secchi / 100,
                         Secchi))
# Seasons based on Polansky et al. 2024, Table 1

# Pivoting for easier time analyzing across response variables
data$finalPivoted <- data$final %>% 
  pivot_longer(c(cf, cf_no_gonad, hsi),
               names_to = "response",
               values_to = "value")

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
# These rows do not have wq data due to being code 9, informed by Claudia


# Begin EDA ---------------------------------------------------------------

# I think this is a good stopping point for now. Will now EDA
# Currently have:
# cf, cf_no_gonad, hsi
# station, MethodCode, Tide, WaterTemperature, Secchi, SpecificConductance,
# TurbidityNTU, DissolveOxygen, DepthBottom, SubRegion, SAC, SJR, OUT, and X2

data$final %>% 
  pivot_longer(c(cf, cf_no_gonad, hsi),
               names_to = "response",
               values_to = "value") %>% 
  ggplot(aes(MethodCode, value, fill = survey_char)) +
  geom_boxplot() +
  facet_wrap(~response, scales = "free_y") +
  labs(title = "hsi different for the SKT")
# Not really a difference between methods, more just between surveys

# response per year
data$final %>% 
  pivot_longer(c(cf, cf_no_gonad, hsi),
               names_to = "response",
               values_to = "value") %>% 
  ggplot(aes(collection_date, value, color = survey_char)) +
  geom_point() +
  facet_wrap(~response, scales = "free_y")
# hsi does decrease over time. Does it appear to be a function of seasonality?
# response per month
data$final %>% 
  pivot_longer(c(cf, cf_no_gonad, hsi),
               names_to = "response",
               values_to = "value") %>% 
  ggplot(aes(month, value, color = survey_char)) +
  geom_jitter() +
  facet_wrap(~response, scales = "free_y")
# does appear to be seasonality driven, the hsi

data$final %>% 
  pivot_longer(c(cf, cf_no_gonad, hsi),
               names_to = "response",
               values_to = "value") %>% 
  ggplot(aes(month, value, fill = survey_char)) +
  geom_boxplot(position = position_dodge(preserve = "single")) +
  facet_wrap(~response)
# hsi appears to be highly influenced by fish maturation
# CF appears to be slightly larger in the FMWT than edsm in the same period. 
# A function of year or method?

data$final %>% 
  pivot_longer(c(cf, cf_no_gonad, hsi),
               names_to = "response",
               values_to = "value") %>% 
  ggplot(aes(season, value, fill = survey_char)) +
  geom_boxplot(position = position_dodge(preserve = "single")) +
  facet_wrap(~response, scale = "free_y")
# In terms of seasonality; summer somewhat higher but strange bump in FMWT for cf

data$final %>% 
  mutate(year = year(collection_date)) %>% 
  pivot_longer(c(cf, cf_no_gonad, hsi),
               names_to = "response",
               values_to = "value") %>% 
  ggplot(aes(factor(year), value, fill = MethodCode)) +
  geom_boxplot() +
  # geom_jitter(aes(color = survey_char), alpha = 0.25) +
  facet_grid(vars(response),
             vars(factor(survey_char, levels = c("fmwt", "stn", "skt", "edsm"))),
             scales = "free_y")
# Seems like fish were larger earlier in the dataset. Only see this in the FMWT though and not
# STN, so likely a function of both seasonlity and year

# As a function of sex
data$final %>% 
  mutate(year = year(collection_date)) %>% 
  ggplot(aes(month, hsi, fill = sex)) +
  geom_boxplot() +
  facet_wrap(~year) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Problem is sex is not recorded for most of 2018 and 2019-2021 data
# Can you approximate using gonad weight...?
data$final %>% 
  transmute(cf, cf_no_gonad, gonad = cf - cf_no_gonad, sex) %>% 
  ggplot(aes(cf, cf_no_gonad, color = gonad)) +
  geom_point() +
  facet_wrap(~sex)
# Can't really predict sex from difference between cf and cf_no_gonad

data$final %>% 
  transmute(cf, cf_no_gonad, gonad = cf - cf_no_gonad, sex,
            collection_date) %>% 
  ggplot(aes(collection_date, gonad, color = sex)) +
  geom_point()

data$final %>% 
  transmute(cf, cf_no_gonad, gonad = cf - cf_no_gonad, sex,
            year = year(collection_date),
            noDifference = gonad == 0) %>%
  group_by(year) %>%
  mutate(sexCount = n()) %>%
  group_by(year) %>% 
  add_count(sex) %>% 
  mutate(proportionSex = n / sexCount) %>% 
  ungroup() %>% 
  {
    proportionNoDifference <- count(., year, noDifference)
    proportionSex <- distinct(., year, sex, proportionSex) %>% 
      filter(is.na(sex))
    
    ggplot(data = proportionNoDifference,
           aes(year, n)) +
      geom_col(aes(fill = noDifference), position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      geom_point(data = proportionSex, aes(y = proportionSex, 
                                           shape = "NA"), size = 3) +
      labs(shape = "Sex Data")
  }
# A bit concerning that the difference between cf and cf_no_gonad is 0 for years
# in which sex data isn't available. Were dissections done for these latest years?
# Is cf_no_gonad actually no gonads or simply cf for these later years?

data$final %>% 
  transmute(cf, cf_no_gonad, gonad = cf - cf_no_gonad, sex,
            month,
            noDifference = gonad == 0) %>%
  group_by(month) %>%
  mutate(sexCount = n()) %>%
  group_by(month) %>% 
  add_count(sex) %>% 
  mutate(proportionSex = n / sexCount) %>% 
  ungroup() %>% 
  {
    proportionNoDifference <- count(., month, noDifference)
    proportionSex <- distinct(., month, sex, proportionSex) %>% 
      filter(is.na(sex))
    
    ggplot(data = proportionNoDifference,
           aes(month, n)) +
      geom_col(aes(fill = noDifference), position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      geom_point(data = proportionSex, aes(y = proportionSex, 
                                           shape = "NA"), size = 3) +
      labs(shape = "Sex Data")
  }
# Turns out, most of the missing sex is due to seasonality; juv fish
# are harder to sex as they have smaller gonads
  
data$final %>% 
  transmute(cf, cf_no_gonad, gonad = cf - cf_no_gonad, sex,
            month, survey_char,
            noDifference = gonad == 0) %>%
  group_by(month) %>%
  mutate(sexCount = n()) %>%
  group_by(month) %>% 
  add_count(sex) %>% 
  mutate(proportionSex = n / sexCount) %>% 
  ungroup() %>% 
  {
    proportionNoDifference <- count(., month, survey_char, noDifference)
    proportionSex <- distinct(., month, sex, survey_char, proportionSex) %>% 
      filter(is.na(sex))
    
    ggplot(data = proportionNoDifference,
           aes(month, n)) +
      geom_col(aes(fill = noDifference), position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      geom_point(data = proportionSex, aes(y = proportionSex, 
                                           shape = "NA"), size = 3) +
      labs(shape = "Sex Data") +
      facet_wrap(~survey_char)
  }
# cf and cf_no_gonads might not be necessary if we're looking at Summer-fall fish (Rosie)
# Perhaps true but I do see some differences. Just need to make sure that these differences
# are truly differences or not...

exploreVariable <- function(variable, transformation = NULL, ...) {
  
  variable <- enquo(variable)
  varName <- as_label(variable)
  
  # Create axis label based on transformation
  axisLabel <- if (!is.null(transformation)) {
    paste0(deparse(substitute(transformation)), "(", varName, ")")
  } else {
    varName
  }
  
  applyTransformation <- function(dataFrame) {
    if (!is.null(transformation)) {
      dataFrame %>% mutate(!!varName := transformation(!!variable))
    } else {
      dataFrame
    }
  }
  
  responseScatter <- data$finalPivoted %>%
    applyTransformation() %>%
    ggplot(aes(.data[[varName]], value)) +
    geom_point(...) +
    geom_smooth(method = "lm") +
    facet_wrap(~response, scales = "free_y") +
    labs(x = axisLabel)
  
  responseSurveyScatter <- data$finalPivoted %>%
    applyTransformation() %>%
    ggplot(aes(.data[[varName]], value, color = survey_char)) +
    geom_point(...) +
    geom_smooth(method = "lm") +
    facet_grid(vars(survey_char), vars(response), scales = "free_y") +
    labs(x = axisLabel)
  
  hsiSurveyScatter <- data$final %>%
    applyTransformation() %>%
    ggplot(aes(.data[[varName]], hsi, color = survey_char)) +
    geom_point(...) +
    facet_wrap(~survey_char, scales = "free_y") +
    labs(x = axisLabel)
  
  hsiYearSurveyScatter <- data$final %>%
    applyTransformation() %>%
    mutate(year = year(collection_date)) %>%
    ggplot(aes(.data[[varName]], hsi, color = survey_char)) +
    geom_point(...) +
    facet_wrap(~year) +
    labs(x = axisLabel)
  
  hsiHeatSeasonality <- data$final %>%
    applyTransformation() %>%
    ggplot(aes(x = month, y = round(hsi, 1), fill = .data[[varName]])) +
    geom_tile() +
    scale_fill_viridis_c(option = "A") +
    labs(fill = axisLabel)
  
  hsiHeatSeasonalitySurvey <- data$final %>%
    applyTransformation() %>%
    ggplot(aes(x = month, y = round(hsi, 1), fill = .data[[varName]])) +
    geom_tile() +
    scale_fill_viridis_c(option = "A") +
    facet_wrap(~survey_char) +
    labs(fill = axisLabel)
  
  hsiHeatSeasonalitySktVsAll <- data$final %>%
    applyTransformation() %>%
    mutate(survey = ifelse(survey_char == "skt", "skt", "stn-edsm-fmwt")) %>%
    ggplot(aes(x = month, y = round(hsi, 1), fill = .data[[varName]])) +
    geom_tile() +
    scale_fill_viridis_c(option = "A") +
    facet_wrap(~survey, scales = "free_y") +
    labs(fill = axisLabel)
  
  cfSurveyScatter <- data$final %>%
    applyTransformation() %>%
    ggplot(aes(.data[[varName]], cf, color = survey_char)) +
    geom_point(...) +
    facet_wrap(~survey_char, scales = "free_y") +
    labs(x = axisLabel)
  
  cfYearSurveyScatter <- data$final %>%
    applyTransformation() %>%
    mutate(year = year(collection_date)) %>%
    ggplot(aes(.data[[varName]], cf, color = survey_char)) +
    geom_point(...) +
    facet_wrap(~year) +
    labs(x = axisLabel)
  
  cfHeatSeasonality <- data$final %>%
    applyTransformation() %>%
    ggplot(aes(x = month, y = round(cf, 2), fill = .data[[varName]])) +
    geom_tile() +
    scale_fill_viridis_c(option = "A") +
    labs(fill = axisLabel)
  
  cfHeatSeasonalitySurvey <- data$final %>%
    applyTransformation() %>%
    ggplot(aes(x = month, y = round(cf, 2), fill = .data[[varName]])) +
    geom_tile() +
    scale_fill_viridis_c(option = "A") +
    facet_wrap(~survey_char) +
    labs(fill = axisLabel)
  
  cfHeatSeasonalitySktVsAll <- data$final %>%
    applyTransformation() %>%
    mutate(survey = ifelse(survey_char == "skt", "skt", "stn-edsm-fmwt")) %>%
    ggplot(aes(x = month, y = round(cf, 2), fill = .data[[varName]])) +
    geom_tile() +
    scale_fill_viridis_c(option = "A") +
    facet_wrap(~survey, scales = "free_y") +
    labs(fill = axisLabel)
  
  cfVsCfNoGonad <- data$finalPivoted %>% 
    filter(response != "hsi") %>% 
    applyTransformation() %>% 
    ggplot(aes(x = month, y = round(value, 2), fill = .data[[varName]])) +
    geom_tile() +
    facet_wrap(~response) +
    scale_fill_viridis_c(option = "A") +
    labs(fill = axisLabel)
  
  list(
    responseScatter,
    responseSurveyScatter,
    hsiSurveyScatter,
    hsiYearSurveyScatter,
    hsiHeatSeasonality,
    hsiHeatSeasonalitySurvey,
    hsiHeatSeasonalitySktVsAll,
    cfSurveyScatter,
    cfYearSurveyScatter,
    cfHeatSeasonality,
    cfHeatSeasonalitySurvey,
    cfHeatSeasonalitySktVsAll,
    cfVsCfNoGonad
  ) %>% setNames(c(
    "responseScatter",
    "responseSurveyScatter",
    "hsiSurveyScatter",
    "hsiYearSurveyScatter",
    "hsiHeatSeasonality",
    "hsiHeatSeasonalitySurvey",
    "hsiHeatSeasonalitySktVsAll",
    "cfSurveyScatter",
    "cfYearSurveyScatter",
    "cfHeatSeasonality",
    "cfHeatSeasonalitySurvey",
    "cfHeatSeasonalitySktVsAll",
    "cfVsCfNoGonad"
  ))
}

walkThroughPlots <- function(list) {
  for (i in list) {
    print(i)
    readline("next")
  }
}

eda <- list()

# --- Water temperature ---
eda$waterTemperature <- exploreVariable(WaterTemperature, alpha = 0.35)
walkThroughPlots(eda$waterTemperature)

# Maybe a diference between hsi past a certain temp threshold? Need to check against seasonaility
# Doesn't seem like temperature affects HSI much; theoretically, only way is if it dictates survival

# Perhaps cf is negatively correlated?
# Really weak negatively correlated; likely a combination of temp + seasonality needed
# cf_no_gonad probably similar, esp in the summer/fall time
# the cf_no_gonad has a shallower slope, likely because it removes confounding effect
# of maturation

# --- Secchi ---
eda$secchi <- exploreVariable(Secchi, alpha = 0.35)
walkThroughPlots(eda$secchi)

# Slight negative relationship with hsi. likely due to fish spawning in the winter
  # when turbidity is generally higher
# When looking at per survey, skt and fmwt have negative, 
  # while stn and edsm has no real relationship, so def seasonlity
# The STN has some really low secchi values and yet it's hsi is just as low as edsm/fmwt
# See no real relationship with cf

# --- Tide ---
eda$tide <- data$finalPivoted %>% 
  filter(!is.na(Tide)) %>% 
  ggplot(aes(Tide, value, fill = survey_char)) +
  geom_boxplot(position = position_dodge(preserve = "single")) +
  facet_wrap(~response, scales = "free_y")
# Don't see much

# --- Specific conductance ---
eda$specificConductance <- exploreVariable(SpecificConductance, alpha = 0.35)
walkThroughPlots(eda$specificConductance)
# Can likely benefit from turning specific conductance to salinity...
# Or can try log transforming it

eda$specificConductanceLog <- exploreVariable(SpecificConductance, transformation = log,
                                           alpha = 0.35)
walkThroughPlots(eda$specificConductanceLog)
# Likely a better predictor here
# Some increased correlation to cf (+) and hsi (-)
# Driven mostly by catches of adults in fresher water as they are spawning

# Slightly negative relationship for hsi; nothing really for cf
  # Only really see this for the SKT and STN

# --- Turbidity ---
eda$TurbidityNTU <- exploreVariable(TurbidityNTU, alpha = 0.35)
walkThroughPlots(eda$TurbidityNTU)
# Likely need to transform this variable; some really high values


# only a few large values of turbidity. Can try to transform it
eda$TurbidityNTULog <- exploreVariable(TurbidityNTU, transformation = log, alpha = 0.35)
walkThroughPlots(eda$TurbidityNTULog)
# No real trends, negative for stn but slightly positive for others
  # Looking at this, it seems like very few catch of individuals above log(4.5) for STN
  # These individuals seemed to have come from 2017, which turbidity was really high

# --- DissolvedOxygen ---
eda$DissolvedOxygen <- exploreVariable(DissolvedOxygen, alpha = 0.35)
walkThroughPlots(eda$DissolvedOxygen)
# Negative correlation to cf?
# Only EDSM takes DO

# --- DepthBottom ---
eda$DepthBottom <- exploreVariable(DepthBottom, alpha = 0.35)
walkThroughPlots(eda$DepthBottom)
# Perhaps some positive correlation, deeper = higher
  # Might only be a midwater phenomenon
# Really strange distribution for EDSM...Most DS (except 1) is collected
  # from the KDTR.
# Across the distribution, too many shallow values in edsm. If you take out 
  # this, there likely is no longer a positive correlation. Survey dependent
# The STN has some really low values too; perhaps the older fish are deeper down?

# --- SubRegion ---
# Coarse attempt to order the subregions
distanceFromGoldenGate <- deltamapr::R_EDSM_Subregions_Mahardja_FLOAT %>% 
  st_transform(crs = 3310) %>% 
  {
    subregions <- .
    # Golden gate approximated point
    goldenGateBridge <- st_sfc(st_point(c(-122.47859198934283, 37.82006342466039)), 
                               crs = 4326) %>% 
      st_transform(crs = 3310)
    
    subregions %>% 
      mutate(minDistToGGB = st_distance(geometry, goldenGateBridge),
             minDistToGGB_km = as.numeric(minDistToGGB) / 1000)
  }

eda$subregion$hsi <- data$final %>% 
  left_join(distanceFromGoldenGate, by = "SubRegion") %>% 
  ggplot(aes(reorder(SubRegion, minDistToGGB_km), hsi, fill = survey_char)) +
  geom_boxplot() +
  facet_wrap(~survey_char, scales = "free_x") +
  coord_flip()

eda$subregion$cf <- data$final %>% 
  left_join(distanceFromGoldenGate, by = "SubRegion") %>% 
  ggplot(aes(reorder(SubRegion, minDistToGGB_km), cf, fill = survey_char)) +
  geom_boxplot() +
  facet_wrap(~survey_char, scales = "free_x") +
  coord_flip()

eda$subregion$cf_no_gonad <- data$final %>% 
  left_join(distanceFromGoldenGate, by = "SubRegion") %>% 
  ggplot(aes(reorder(SubRegion, minDistToGGB_km), cf_no_gonad, fill = survey_char)) +
  geom_boxplot() +
  facet_wrap(~survey_char, scales = "free_x") +
  coord_flip()

# Difficult to tell

# --- Sac ---
eda$SAC <- exploreVariable(SAC, alpha = 0.35)
walkThroughPlots(eda$SAC)
# Seems like positive correlation to hsi; however, might be year based
# Driven mainly by skt catches in 2017
# nothing really for cf

eda$SACLog <- exploreVariable(SAC, alpha = 0.35, transformation = log)
walkThroughPlots(eda$SACLog)
# positive in EDSM (for hsi) but negative in stn

# --- SJR ---
eda$SJR <- exploreVariable(SJR, alpha = 0.35)
walkThroughPlots(eda$SJR)
# Similiar relationship as SAC in which edsm shows positive and stn (and perhaps fmwt) show negative

eda$SJRLog <- exploreVariable(SJR, alpha = 0.35, transformation = log)
walkThroughPlots(eda$SJRLog)
# log relationship actually shows a negative to hsi here; likely supports no relationship

# --- OUT ---
eda$OUT <- exploreVariable(OUT, alpha = 0.35)
walkThroughPlots(eda$OUT)
# positive correlation to hsi; likely driven by a singular year
# Driven by skt, higher hsi values of larger individuals in 2017, although those hsi aren't particularly more
# than other years in skt
# Likely due to so many "low" hsi values from the other surveys vs only having higher hsi
# for high outflow only from the skt

eda$OUTLog <- exploreVariable(OUT, alpha = 0.35, transformation = log)
walkThroughPlots(eda$OUTLog)
# Can't really see anything for cf

# --- X2 ---
eda$X2 <- exploreVariable(X2, alpha = 0.35)
walkThroughPlots(eda$X2)
# Like outflow, higher hsi in higher flow years by the skt causing the relationship to be negatively correlated

# --- Month ---
eda$month$hsi <- data$final %>% 
  ggplot(aes(month, hsi)) +
  geom_boxplot() +
  facet_wrap(~survey_char, scales = "free_y")
# Heavily influenced by seasonlity, esp in skt

eda$month$cf <- data$final %>% 
  ggplot(aes(month, cf)) +
  geom_boxplot() +
  facet_wrap(~survey_char, scales = "free_y")
# Weird CF values in Nov...How many years was this?

eda$month$cf_no_gonad <- data$final %>% 
  ggplot(aes(month, cf_no_gonad)) +
  geom_boxplot() +
  facet_wrap(~survey_char, scales = "free_y")

eda$timeSampled <- data$final %>% 
  distinct(month, year = year(collection_date)) %>% 
  count(month, name = "numberYearsSampled") %>% 
  left_join(data$final %>% 
              count(month, name = "numberDeltaSmeltCaught"),
            by = "month") %>% 
  ggplot(aes(month, numberYearsSampled)) +
  geom_col() +
  geom_text(aes(label = paste0("ds = ", numberDeltaSmeltCaught)), 
            vjust = -1, size = 5, color = "firebrick")

# --- season ---
eda$season$hsi <- data$final %>% 
  ggplot(aes(season, hsi)) +
  geom_boxplot() +
  facet_wrap(~survey_char, scales = "free_y")

eda$season$cf <- data$final %>% 
  ggplot(aes(season, cf)) +
  geom_boxplot() +
  facet_wrap(~survey_char, scales = "free_y")

eda$season$cf_no_gonad <- data$final %>% 
  ggplot(aes(season, cf_no_gonad)) +
  geom_boxplot() +
  facet_wrap(~survey_char, scales = "free_y")
# Metrics higher in the summer vs fall in edsm

# Summary EDA Pass 1 ------------------------------------------------------
# Analysis of metrics at the time of measuring is not useful. Also not useful for our analysis
# in which we want to compare conditions in a season to another. However, I wanted to see anyways

# EDA Aggregations and Lags -----------------------------------------------

# Fundamental concept: anything beyond optimal conditions means energy taken away from 
  # growth or recruitment

# Will try 3, 7, 14, 21, and monthly aggregations
# Will also try seasonal and yearly aggregations
  # For these longer ones, spring conditions might affect: 
    # health of adults and thus egg quality
    # early survival/growth of juveniles hatching
    # any residual effects of spring conditions to food item growth into the summer
    # spring conditions may allow individuals to occupy more favorable habitat that then
      # gets cut off during the summer, e.g., Suisun Marsh

# --- Calculate wq data per subregion ---
data$cdecMetadata <- deltadata:::cdecStations %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(crs = 3310) %>% 
  st_join(deltamapr::R_EDSM_Subregions_Mahardja_FLOAT %>% 
            st_transform(crs = 3310), join = st_intersects) %>% 
  # Bounding to just the delta
  filter(st_intersects(geometry, st_as_sfc(st_bbox(distanceFromGoldenGate)), 
                       sparse = FALSE)[,1])

distanceFromGoldenGate %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = data$cdecMetadata %>% 
            filter(!is.na(SubRegion)),
          aes(color = SubRegion),
          size = 2) +
  geom_sf(data = data$cdecMetadata %>% 
            filter(is.na(SubRegion)),
          aes(color = SubRegion),
          size = 1, alpha = 0.25)

# Don't really have alot of data in the more ds regions. Might have to incorporate
# NOAA gauges.

# Misc --------------------------------------------------------------------

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



  

