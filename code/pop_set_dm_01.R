library(haven)
library(tidyverse)
library(janitor)

# Import data
pilot_data_raw <- read_sav("data/PopSter_+1st+Pilot_June+6,+2025_10.12.sav")

pilot <- pilot_data_raw %>%
  janitor::clean_names() %>%
  filter(finished == 1) %>%
  mutate(pid = 1:n())


# Variables

## Demographics
pilot <- pilot %>% 
  mutate(duration_in_minutes = (duration_in_seconds/60) %>% round(1)) %>% 
  mutate(gender = qid6,
         education = qid8,
         age_group = qid7,
         sector = dplyr::coalesce(qid86,q200),
         religiosity = coalesce(qid87,q201),
         district = coalesce(qid88,q202),
         income = dplyr::coalesce(qid89,q203) %>% na_if(9),
         subjective_social_economic_status = dplyr::coalesce(qid90,q204) %>% na_if(9),
         ethnic_identity = dplyr::coalesce(qid96,q209),
         born_abroad = dplyr::coalesce(qid97,q210),
         sector_employment = dplyr::coalesce(qid92,q205),
         public_sector_past = dplyr::coalesce(qid93,q206),
         ) %>% 
  mutate(female = ifelse(gender==2,1,0)) %>% 
  mutate(public_sector_employee_current = ifelse(sector_employment==1, 1,0),
         public_sector_experience = ifelse(sector_employment==1 | public_sector_past==1, 1,0)) %>% 


# Political variables

  mutate(external_efficecy = dplyr::coalesce(q194,q195) %>% na_if(5) %>% 
           dplyr::recode(`1`=4,
                         `2`=3,
                         `3`=2,
                         `4`=1) %>% labelled::remove_labels(), 
          right_left_ideology = dplyr::coalesce(q157,q188) %>%  na_if(8),
         party_vote_past = dplyr::coalesce(q158,q189),
         party_vote_intention = dplyr::coalesce(q159,q190),
         political_block = dplyr::coalesce(q160,q191),
         affect_right = dplyr::coalesce(q161_1,q192_1),
         affect_left = dplyr::coalesce(q162_1,q193_1)
  ) %>% 

# Media
  mutate(
    watch_tv = dplyr::coalesce(q197_1, q196_1),
    watch_ch14 = dplyr::coalesce(q150_4, q194_4) %>% replace_na(0)) %>% 



# Populist attitudes
  mutate(
    pop1 = dplyr::coalesce(q125_1, q184_1),
    pop2 = dplyr::coalesce(q125_2, q184_2),
    pop3 = dplyr::coalesce(q125_3, q184_3),
    pop4 = dplyr::coalesce(q125_4, q184_4),
    pop5 = dplyr::coalesce(q125_5, q184_5),  
    pop6 = dplyr::coalesce(q125_6, q184_6),
    pop7 = dplyr::coalesce(q125_7, q184_7),
    pop8 = dplyr::coalesce(q125_8, q184_8),
    pop9 = dplyr::coalesce(q125_9, q184_9),
    pop10 = dplyr::coalesce(q125_10, q184_10),
    pop11 = dplyr::coalesce(q125_11, q184_11),
    pop12 = dplyr::coalesce(q125_12, q184_12),
    pop13 = dplyr::coalesce(q125_13, q184_13),
    pop14 = dplyr::coalesce(q125_14, q184_14),
    pop15 = dplyr::coalesce(q125_15, q184_15),
    pop16 = dplyr::coalesce(q125_16, q184_16),
    pop17 = dplyr::coalesce(q125_17, q184_17),
    pop18 = dplyr::coalesce(q125_18, q184_18),
    pop19 = dplyr::coalesce(q125_19, q184_19),
    pop20 = dplyr::coalesce(q125_20, q184_20)
  ) %>%
  mutate(
    across(
      .cols = pop1:pop20,
      .fns = ~ labelled::remove_labels(.x),
      .names = "{.col}_n"
    )
  ) %>%
  mutate(pop5_n = pop5_n %>% 
           dplyr::recode(`1`=5,
                         `2`=4,
                         `3`=3,
                         `4`=2,
                         `5`=1,))  


pilot_short <- pilot %>% dplyr::select(response_id,
                                       duration_in_minutes:pop20)
           

# responses dataset

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

#readr::write_excel_csv(responses_civil_servants_direction,"responses_civil servants.csv")



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





