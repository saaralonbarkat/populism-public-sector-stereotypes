library(haven)
library(tidyverse)
library(janitor)


# Import data
wave2_data_raw <- read_sav("data/PopSter_wave_2.sav")


wave2 <- wave2_data_raw %>%
  janitor::clean_names() %>%
  filter(qid4==1) %>% 
  mutate(pid_w2 = 1:n()) |> 
  
  mutate(duration_in_minutes_w2 = (duration_in_seconds/60) %>% round(1)) %>% 
# Randomization labels
  
  mutate(civil_servant_label = case_when(
    a1_q119_1==1 ~ "public service employee",
    a2_q119_1==1 ~ "public service bureaucrat",
    a3_q119_1==1 ~ "public service clerk"
  )) |> 
  mutate(entity_2 = case_when(
    a1_q59_1==1 ~ "Employees in Finance",
    a2_q59_1==1 ~ "Employees in Agriculture",
    a3_q59_1==1 ~ "Employees in High education"
  )) |> 
  mutate(entity_3 = case_when(
    a1_q69_1 == 1 ~ "Public hospital doctors",
    a2_q69_1 == 1 ~ "Public hospital administrators",
    a3_q69_1 == 1 ~ "Public school teachers",
    a4_q69_1 == 1 ~ "Education Ministry inspectors",
    a5_q69_2 == 1 ~ "Traffic police officers",
    a6_q69_1 == 1 ~ "Police investigators",
    a7_q69_1 == 1 ~ "Tax inspectors",
    a8_q69_1 == 1 ~ "Tax authority service employees",
    TRUE ~ NA_character_
  )
) |> 
  mutate(imagined_entity = stringr::str_c(a1_qid31, a2_qid31, a3_qid31)) |> 

  mutate(
  friendly_scale        = q61_1,
  sociable_scale        = q61_2,
  trustworthy_scale     = q61_3,
  fair_scale            = q61_4,
  competent_scale       = q61_5,
  skilled_scale         = q61_6,
  confident_scale       = q61_7,
  assertive_scale       = q61_8,
  goal_oriented_scale   = q61_9,
  high_status_scale     = q61_10,
  respected_scale       = q61_11,
  economically_secure_scale = q61_12
) |> 
mutate(
  religious    = q37_1,
  conservative = q37_2,
  liberal      = q37_3,
  ashkenazi    = q37_4,
  traditional      = q37_5
) |> 
  mutate(general_image_personal = q21_1,
general_image_social = q24_1,
civil_servant_elite_wave2 = q57,
trust_civil_service_wave2 = q40,
public_private_sector_performance = q38
  ) |> 
  mutate(
  politicization_1 = q41_1,
  politicization_2 = q41_2,
  politicization_3 = q41_3,
  politicization_4 = q41_4,
  politicization_5 = q41_5,
  politicization_6 = q41_6
) |> 
  mutate(public_service_work_information = case_when(
    q33==1 ~1,
    q33==2 ~0,
    progress==100 ~0,
  )) |> 
  mutate(completed_wave2 = ifelse(progress==100,1,0),
completed_stereotypes_wave2 = ifelse(progress>=53,1,0))



waves_combined <- wave1 |> dplyr::select(i_user3,pid,gender:psm_aps) |> 
  left_join(wave2 |> dplyr::select(i_user3,pid_w2:completed_stereotypes_wave2,progress), by = "i_user3") |> 
  mutate(return_wave2 = ifelse(is.na(pid_w2),0,1),
completed_wave2 = ifelse(completed_wave2==1,1,0) |> replace_na(0),
completed_stereotypes_wave2 = ifelse(completed_stereotypes_wave2==1,1,0) |> replace_na(0)
) 




# responses dataset

## responses dataset public service employees (1) # עובדים בשירות הציבורי
responses_public_service_employees <- wave2 %>% 
  dplyr::select(response_id,
                pid_w2,
                a1_q119_1_text:a1_qid31,civil_servant_label) %>% 
  pivot_longer(cols = a1_q119_1_text:a1_q119_6_text,
               values_to = "response") %>% 
  mutate(response_order = name %>% 
           recode(
             `a1_q119_1_text` = 1,
             `a1_q119_2_text` = 2,
             `a1_q119_3_text` = 3,
             `a1_q119_4_text` = 4,
             `a1_q119_5_text` = 5,
             `a1_q119_6_text` = 6
           ),
         imagined_entity = a1_qid31) %>% 
  mutate(participant_response_id = str_c(pid_w2,"_",response_order)) %>% 
  select(response_id,
         pid_w2,
         civil_servant_label,
         response_order,
         participant_response_id,
         response,
         imagined_entity) %>% 
  filter(response!="")


## responses dataset  public service bureaucrats (2) # בירוקרטים בשירות הציבורי
responses_public_service_bureaucrats <- wave2 %>% 
  dplyr::select(response_id,
                pid_w2,
                a2_q119_1_text:a2_qid31,civil_servant_label) %>% 
  pivot_longer(cols = a2_q119_1_text:a2_q119_6_text,
               values_to = "response") %>% 
  mutate(response_order = name %>% 
           recode(
             `a2_q119_1_text` = 1,
             `a2_q119_2_text` = 2,
             `a2_q119_3_text` = 3,
             `a2_q119_4_text` = 4,
             `a2_q119_5_text` = 5,
             `a2_q119_6_text` = 6
           ),
         imagined_entity = a2_qid31) %>% 
  mutate(participant_response_id = str_c(pid_w2,"_",response_order)) %>% 
  select(response_id,
         pid_w2,
         civil_servant_label,
         response_order,
         participant_response_id,
         response,
         imagined_entity) %>% 
  filter(response!="")



## responses dataset clercks (3) # פקידים בשירות הציבורי
responses_public_service_clerks <- wave2 %>% 
  dplyr::select(response_id,
                pid_w2,
                a3_q119_1_text:a3_qid31,civil_servant_label) %>% 
  pivot_longer(cols = a3_q119_1_text:a3_q119_6_text,
               values_to = "response") %>% 
  mutate(response_order = name %>% 
           recode(
             `a3_q119_1_text` = 1,
             `a3_q119_2_text` = 2,
             `a3_q119_3_text` = 3,
             `a3_q119_4_text` = 4,
             `a3_q119_5_text` = 5,
             `a3_q119_6_text` = 6
           ),
         imagined_entity = a3_qid31) %>% 
  mutate(participant_response_id = str_c(pid_w2,"_",response_order)) %>% 
  select(response_id,
         pid_w2,
         civil_servant_label,
         response_order,
         participant_response_id,
         response,
         imagined_entity) %>% 
  filter(response!="")


responses_direction <- wave2 %>% 
  dplyr::select(response_id,
                pid_w2,
                q123_1:q123_6,civil_servant_label) %>% 
  pivot_longer(cols = q123_1:q123_6,
               values_to = "response_direction") %>% 
  mutate(response_order = name %>% 
           recode(
             `q123_1` = 1,
             `q123_2` = 2,
             `q123_3` = 3,
             `q123_4` = 4,
             `q123_5` = 5,
             `q123_6` = 6
           )) %>% 
  mutate(participant_response_id = str_c(pid_w2,"_",response_order)) %>% 
  select(response_id,
         pid_w2,
         civil_servant_label,
         response_order,
         participant_response_id,
         response_direction) %>%
  drop_na(response_direction)


responses_direction_wave2 <- bind_rows(responses_public_service_employees,
                                                responses_public_service_bureaucrats,
                                                responses_public_service_clerks,
                                                ) %>% 
  left_join(responses_direction)






save.image()

waves_combined |> filter(civil_servant_label %in% NA) |> dplyr::select(pid, i_user3) |> write_csv("no_return_user_id_15.02.2026.csv")
