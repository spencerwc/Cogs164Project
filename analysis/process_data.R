library(here)
library(tidyverse)
library(jsonlite)

processed_data_directory <- here("..","data","processed_data")
file_name <- "statistical_learning"

#read experiment data
exp_data <- read_csv(here(processed_data_directory,paste0(file_name,"-alldata.csv"))) %>%
  rename(participant_id=participant)

#double check that participant ids are unique
counts_by_random_id <- exp_data %>%
  group_by(random_id,participant_id) %>%
  count()
#output to track participants
write_csv(counts_by_random_id,here(processed_data_directory,paste0(file_name,"-participant-list.csv")))

#filter and select relevant data
processed_data <- exp_data %>%
  filter(!is.na(test_triplet)) %>%
  select(participant_id, random_id,trial_index,time_elapsed,test_triplet:secondSequencePaths,response,rt,correct_response,correct) %>%
  group_by(participant_id) %>%
  mutate(
    trial_number=seq(n())
  ) %>%
  relocate(
    trial_number,.after="trial_index"
  ) %>%
  mutate(
    is_right = ifelse(correct,1,0)
  ) %>%
  relocate(
    is_right,.after="correct"
  ) %>%
  #make rt numeric
  mutate(
    rt=as.numeric(as.character(rt))
  )

#store processed and prepped data
write_csv(processed_data,here(processed_data_directory,paste0(file_name,"-processed-data.csv")))
