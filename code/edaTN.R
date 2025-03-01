library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(tidyr)

data <- list()

options(scipen = 99999)
theme_set(theme_bw(base_size = 22))

# Read in the files -------------------------------------------------------
data$health <- read_excel("Data/Master_ordered_srdwsc_zoop.xlsx", sheet = "Master_ordered")
data$metadata <- read_excel("Data/Master_ordered_srdwsc_zoop.xlsx", sheet = "METADATA")

# Document and Expand Metadata --------------------------------------------

# --- Available vs described columns ---
# There's a row in the medata that outlines the classes of zooplankton from column 3+
# Removing this for now
metadataNew <- metadata %>% 
  transmute(
    variable = `BASIC INFO`,
    decriptionExisting = `...2`,
    presentInMetadata = T
  ) %>% 
  full_join(
    data.frame(
      variable = names(health),
      presentInData = T
    ),
    by = "variable"
  ) %>% 
  mutate(
    presentInBoth = ifelse(presentInMetadata & presentInData, T, F)
  )

# Start present in metadata but missing in health
metadataNew %>% 
  filter(is.na(presentInData) & presentInMetadata) %>% 
  View()

# Present in health but missing in metadata
metadataNew %>% 
  filter(is.na(presentInMetadata) & presentInData) %>% 
  pull(variable) %>% 
  {
    select(health, all_of(.))
  } %>% 
  View()

# Present in both
metadataNew %>% 
  filter(presentInData & presentInMetadata) %>% 
  View()

data$metadataNew <- read_xlsx("metadata.xlsx")

# Create full metadata combining existing with new
data$metadataFinal <- data$metadata %>% 
  transmute(variableJoin = `BASIC INFO`,
            descriptionExisting = `...2`,
            presentInMetadata = T) %>% 
  filter(!is.na(variableJoin)) %>% 
  full_join(
    data$metadataNew %>% 
      # If a column in the data has metadata but is just named differently from the value in the metadata file,
      # use the metadata name to join in the respective metadata
      mutate(useMetadataName = ifelse(!is.na(presentInMetadata) & !is.na(presentInData) & (presentInMetadata != presentInData),
                                      T, F)) %>% 
      transmute(variable = presentInData,
                variableJoin = ifelse(useMetadataName, presentInMetadata, variable),
                notes,
                description,
                descriptionExisting,
                type),
    by = c("variableJoin", "descriptionExisting")
  ) %>% 
  mutate(inData = ifelse(variable %in% names(data$health), T, F), .after = variable) %>% 
  filter(inData) %>% 
  transmute(
    variable, descriptionExisting, description, notes, inData, type 
  ) %>% 
  # Total NAs
  left_join(
    colSums(is.na(data$health)) %>% 
      data.frame(variable = names(.),
                 numberNas = .,
                 numberNasProportion = ./nrow(data$health),
                 row.names = NULL),
    by = "variable"
  ) %>% 
  # Mean yearly NAs
  left_join(
    data$health %>%
      group_by(year = year(collection_date)) %>%
      summarise(across(everything(), ~sum(is.na(.))/nrow(data$health), 
                       .names = "{.col}")) %>%
      pivot_longer(
        cols = -year,
        names_to = "variable",
        values_to = "naCount"
      ) %>%
      filter(!is.na(year)) %>% 
      group_by(variable) %>% 
      summarise(naCountYearlyMean = mean(naCount)),
    by = "variable"
  ) %>% 
  as.data.frame()

# --- Visualize the metadata ---
data$metadataFinal %>% 
  count(type) %>% 
  ggplot(aes(reorder(type, -n), n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -1, size = 5) + 
  labs(y = "NA Counts", x = "Variable Usefulness")
# 106 variables to ignore, about 1/3 of the variable

# Of the remaining variables, how bad are the NAs
data$metadataFinal %>% 
  filter(type != "ignore") %>% 
  distinct(variable, numberNasProportion) %>% 
  ggplot(aes(reorder(variable, numberNasProportion), numberNasProportion)) +
  geom_col()
# Need to now look through and see which ones are most useful using this updated metadata

write.csv(data$metadataFinal %>% 
            filter(type != "ignore") %>% 
            arrange(numberNas, variable),
          "consideredNas.csv", 
          row.names = F)
# Seems like NAs proportions less than 0.215372907 and 0.360350076 are my cutoffs

write.csv(data$metadataFinal %>% 
            filter(numberNasProportion <= 0.360350076 + .Machine$double.eps) %>% 
            arrange(numberNas, variable),
          "consideringThreshold.csv", 
          row.names = F)

read.csv("consideredNas.csv", na.strings = "") %>% 
  filter(!is.na(typeAfterNas)) %>% 
  arrange(factor(typeAfterNas, levels = c("response", "engineer", "predictor"))) %>% 
  select(-type) %>% 
  write.csv("needMetadata.csv",
            row.names = F)

# --- Data distribution ---
# Outliers?

# --- Correlation between metrics ---

