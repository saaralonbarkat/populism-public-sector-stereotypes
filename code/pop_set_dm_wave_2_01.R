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
  mutate(public_service_work_information = ifelse(q33==1,1,0)) |> 
  mutate(completed_wave2 = ifelse(progress==100,1,0),
completed_stereotypes_wave2 = ifelse(progress>=53,1,0))



waves_combined <- wave1 |> dplyr::select(i_user3,pid,gender:psm_aps) |> 
  left_join(wave2 |> dplyr::select(i_user3,pid_w2:completed_stereotypes_wave2), by = "i_user3") |> 
  mutate(return_wave2 = ifelse(is.na(pid_w2),0,1),
completed_wave2 = ifelse(completed_wave2==1,1,0) |> replace_na(0),
completed_stereotypes_wave2 = ifelse(completed_stereotypes_wave2==1,1,0) |> replace_na(0)
) 


save.image()

