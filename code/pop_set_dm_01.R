library(haven)
library(tidyverse)
library(janitor)

#Import data
pilot_data_raw <- read_sav("data/PopSter_+1st+Pilot_June+6,+2025_10.12.sav")

pilot <- pilot_data_raw %>% janitor::clean_names() %>% 
  filter(finished==1) %>% 
  mutate(pid = 1:nrow(pilot)) 

#responses dataset

## responses dataset general
responses_civil_servants <- pilot %>% 
  dplyr::select(response_id,
                pid,
                q124_1_text:qid21) %>% 
  pivot_longer(cols = q124_1_text:q124_6_text,
               values_to = "response") %>% 
  mutate(entity = "civil servants",
         response_order = name %>% 
           recode(
             `q124_1_text` = 1,
             `q124_2_text` = 2,
             `q124_3_text` = 3,
             `q124_4_text` = 4,
             `q124_5_text` = 5,
             `q124_6_text` = 6
           ),
         imagined_entity = qid21) %>% 
  mutate(participant_response_id = str_c(pid,"_",entity,"_",response_order)) %>% 
  select(response_id,
         pid,
         entity,
         response_order,
         participant_response_id,
         response,
         imagined_entity) %>% 
  filter(response!="")


responses_civil_servants_direction <- pilot %>% 
  dplyr::select(response_id,
                pid,
                q123_1:q123_6) %>% 
  pivot_longer(cols = q123_1:q123_6,
               values_to = "response_direction") %>% 
  mutate(entity = "civil servants",
         response_order = name %>% 
           recode(
             `q123_1` = 1,
             `q123_2` = 2,
             `q123_3` = 3,
             `q123_4` = 4,
             `q123_5` = 5,
             `q123_6` = 6
           )) %>% 
  mutate(participant_response_id = str_c(pid,"_",entity,"_",response_order)) %>% 
  select(response_id,
         pid,
         entity,
         response_order,
         participant_response_id,
         response_direction) %>%
  drop_na(response_direction)


responses_civil_servants_direction <- responses_civil_servants %>% 
  left_join(responses_civil_servants_direction)

readr::write_excel_csv(responses_civil_servants_direction,"responses_civil servants.csv")



## responses dataset senior
responses_senior_civil_servants <- pilot %>% 
  dplyr::select(response_id,
                pid,
                a1_q119_1_text:a1_qid31) %>% 
  pivot_longer(cols = a1_q119_1_text:a1_q119_6_text,
               values_to = "response") %>% 
  mutate(entity = "senior civil servants",
         response_order = name %>% 
           recode(
             `a1_q119_1_text` = 1,
             `a1_q119_2_text` = 2,
             `a1_q119_3_text` = 3,
             `a1_q119_4_text` = 4,
             `a1_q119_5_text` = 5,
             `a1_q119_6_text` = 6
           ),
         imagined_entity = a1_qid31) %>% 
  mutate(participant_response_id = str_c(pid,"_",entity,"_",response_order),
         participant_response_id_2 = str_c(pid,"_","senior_junior","_",response_order)) %>% 
  select(response_id,
         pid,
         entity,
         response_order,
         participant_response_id,
         participant_response_id_2,
         response,
         imagined_entity) %>% 
  filter(response!="")



## responses dataset junior
responses_junior_civil_servants <- pilot %>% 
  dplyr::select(response_id,
                pid,
                a2_q119_1_text:a2_qid31) %>% 
  pivot_longer(cols = a2_q119_1_text:a2_q119_6_text,
               values_to = "response") %>% 
  mutate(entity = "junior civil servants",
         response_order = name %>% 
           recode(
             `a2_q119_1_text` = 1,
             `a2_q119_2_text` = 2,
             `a2_q119_3_text` = 3,
             `a2_q119_4_text` = 4,
             `a2_q119_5_text` = 5,
             `a2_q119_6_text` = 6
           ),
         imagined_entity = a2_qid31) %>% 
  mutate(participant_response_id = str_c(pid,"_",entity,"_",response_order),
         participant_response_id_2 = str_c(pid,"_","senior_junior","_",response_order)) %>% 
  select(response_id,
         pid,
         entity,
         response_order,
         participant_response_id,
         participant_response_id_2,
         response,
         imagined_entity) %>% 
  filter(response!="")


responses_senior_junior_civil_servants_direction <- pilot %>% 
  dplyr::select(response_id,
                pid,
                q123_7:q123_12) %>% 
  pivot_longer(cols = q123_7:q123_12,
               values_to = "response_direction") %>% 
  mutate(response_order = name %>% 
           recode(
             `q123_7` = 1,
             `q123_8` = 2,
             `q123_9` = 3,
             `q123_10` = 4,
             `q123_11` = 5,
             `q123_12` = 6
           )) %>% 
  mutate(participant_response_id_2 = str_c(pid,"_","senior_junior","_",response_order)) %>% 
  select(response_id,
         pid,
         response_order,
         participant_response_id_2,
         response_direction) %>%
  drop_na(response_direction)


responses_junior_senior_civil_servants_direction <- bind_rows(responses_senior_civil_servants, 
                                                              responses_junior_civil_servants) %>% 
  left_join(responses_senior_junior_civil_servants_direction)


responses_civil_servants_all_direction <- bind_rows(responses_civil_servants_direction,
                                                    responses_junior_senior_civil_servants_direction)

readr::write_excel_csv(responses_civil_servants_all_direction,"responses_civil servants_all.csv")




