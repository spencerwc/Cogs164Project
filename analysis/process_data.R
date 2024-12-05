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

#filter participant ids
filter_ids <- c(
  "1","12","1234","a1","as","nm","p4","p6"
)

processed_data <- processed_data %>%
  mutate(participant_id = trimws(tolower(participant_id))) %>%
  #fix some ids
  mutate(
    participant_id = case_when(
      participant_id == "herson" ~ "heron",
      participant_id == "p73" ~ "giraffe",
      participant_id == "2341" ~ "porcupine",
      participant_id == "a17689315" ~ "rabbit",
      TRUE ~ participant_id
    )
  ) %>%
  filter(!(participant_id %in% filter_ids))

#store processed and prepped data
write_csv(processed_data,here(processed_data_directory,paste0(file_name,"-processed-data.csv")))
