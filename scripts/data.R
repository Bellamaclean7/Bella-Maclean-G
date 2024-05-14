#### Preamble ####
# Purpose: Test
# Author: Bella macLean
# Date: 14 May 2024
# Contact: bellamaclean7@gmail.com


#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)

# Task One: Read in the csv files and perform de-identification
admissions_surg <- read_csv(here::here("data/admissions_surg.csv"))
admissions_med <- read_csv(here::here("data/admissions_med.csv"))
imaging <- read_csv(here::here("data/imaging.csv"))

# Compile a list of IDs
all_ids <- unique(c(admissions_surg$ID, admissions_med$ID))

# Create a dataframe with unique identifiers
q1b <- data.frame(ID = all_ids, DE_ID = seq_along(all_ids))

# Merge and de-identify datasets
deidentified_surg <- merge(admissions_surg, q1b, by = "ID") |>
  select(DE_ID, everything(), -ID)
deidentified_med <- merge(admissions_med, q1b, by = "ID") |>
  select(DE_ID, everything(), -ID)
deidentified_imaging <- merge(imaging, q1b, by = "ID") |>
  select(DE_ID, everything(), -ID)

# Display the first 5 rows of each de-identified dataset using kable
head(deidentified_surg, 5)
head(deidentified_med, 5)
head(deidentified_imaging, 5)

# Task Two: Combine datasets into one
combined_admissions <- bind_rows(deidentified_surg, deidentified_med)
admissions_img <- left_join(combined_admissions, deidentified_imaging, by = "DE_ID") |>
head()

# Task Three: Calculate length of stay and mean by department
admissions_img <- admissions_img |>
  mutate(
    admission_datetime = as.POSIXct(paste(ADMISSION.DATE, format(ADMISSION.TIME, "%H:%M:%S")), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    discharge_datetime = as.POSIXct(paste(DISCHARGE.DATE, format(DISCHARGE.TIME, "%H:%M:%S")), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    length_of_stay = as.numeric(difftime(discharge_datetime, admission_datetime, units = "days"))
  )
mean_length_of_stay_by_dept <- admissions_img |>
  group_by(DEPARTMENT) |>
  summarise(Mean_Length_of_Stay = mean(length_of_stay, na.rm = TRUE)) |>
  head()

# Task Four: Filter to the first performed test and reformat
q4_df <- imaging |>
  arrange(test_name, performed_date, performed_time) |>
  group_by(test_name) |>
  slice_head(n = 1) |>
  ungroup() |>
  head()



