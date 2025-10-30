library(haven)
library(tidyverse)
library(janitor)


# Import data
pilot2_data_raw <- read_sav("data/PopSter_+2nd+Pilot_August+25,+2025_17.35.sav")


pilot2 <- pilot2_data_raw %>%
  janitor::clean_names() %>%
  drop_na(qid100) %>%
  mutate(pid = 1:n())


## Demographics
pilot2 <- pilot2 %>% 
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
  mutate(affective_polarization = abs(affect_right-affect_left)) %>% 
  mutate(coalition_party_vote_past = case_when(
    party_vote_past %in% c(1,3,5,6) ~ "Netanyahu coalition",
    party_vote_past %in% c(2,4,7,9,10,11) ~ "Anti Netanyahu coalition",
    TRUE ~ "Other"
  )) %>%
    mutate(coalition_party_vote_intention = case_when(
   party_vote_intention %in% c(1,3,5,6,13) ~ "Netanyahu coalition",
    party_vote_intention %in% c(2,4,7,9,10,11) ~ "Anti Netanyahu coalition",
    TRUE ~ "Other"
  )) %>%
  
  
# Populist attitudes
  mutate(
    pop1 = dplyr::coalesce(q125_1, q184_1),#popular sovereignty 1
    pop2 = dplyr::coalesce(q125_2, q184_2),#popular sovereignty 2
    pop3 = dplyr::coalesce(q125_3, q184_3),#popular sovereignty 3
    pop4 = dplyr::coalesce(q125_4, q184_4),#anti elite 1
    pop5 = dplyr::coalesce(q125_5, q184_5),  #anti elite 2
    pop6 = dplyr::coalesce(q125_6, q184_6),#anti elite 3
    pop7 = dplyr::coalesce(q125_7, q184_7),#anti elite 4
    pop8 = dplyr::coalesce(q125_8, q184_8),#Homogeneity 1
    pop9 = dplyr::coalesce(q125_9, q184_9),#Homogeneity 2
    pop10 = dplyr::coalesce(q125_10, q184_10),#Homogeneity 3
    pop11 = dplyr::coalesce(q125_11, q184_11),#Manichean 1
    pop12 = dplyr::coalesce(q125_12, q184_12),#Manichean 2
    pop13 = dplyr::coalesce(q125_13, q184_13)#Manichean 3
  ) %>%
  mutate(
    across(
      .cols = pop1:pop13,
      .fns = ~ labelled::remove_labels(.x),
      .names = "{.col}_n"
    )
  )%>% 

  mutate(popular_sovereignty = (((pop1_n + pop2_n + pop3_n)/3-1)/4),
         anti_elitism        = (((pop4_n + pop5_n + pop6_n + pop7_n)/4-1)/4),
         homogeneity         = (((pop8_n + pop9_n + pop10_n)/3-1)/4),
         manichean           = (((pop11_n + pop12_n + pop13_n)/3-1)/4)) %>% 
  mutate(populist_attitudes_index_4dim = (popular_sovereignty+anti_elitism+homogeneity+manichean)/4,
         populist_attitudes_index_3dim = (popular_sovereignty+anti_elitism+homogeneity)/3) %>% 
  
  
#Trust
  
  mutate(trust_knesset = q202_1,
         trust_government = q202_2,
         trust_judiciary = q202_3,
         trust_health = q202_4,
         trust_police = q202_5,
         trust_municipality = q202_6,
         trust_civil_service = q202_7,
         trust_media = q202_8
         ) %>% 


# Conspiracy theory

  mutate(conspiracy_deep_state = q232,
         conspiracy_deep_state_n = q232 %>% na_if(6),
         conspiracy_vaccine = q235,
         conspiracy_vaccine_n = q235 %>% na_if(6)) %>% 
  
# Randomization labels
  
  mutate(civil_servant_label = case_when(
    a1_q119_1==1 ~ "public service employee",
    a2_q119_1==1 ~ "public service clerk",
    a3_q119_1==1 ~ "bureaucrat",
    a4_q119_1==1 ~ "public service bureaucrat",
    
  ))

pilot2 %>% labelled::remove_labels() %>%  ggplot(aes(x=populist_attitudes_index_3dim))+
  geom_histogram(bins=10)+
  facet_wrap(vars(trust_civil_service))

pilot2 %>% labelled::remove_labels() %>%  ggplot(aes(x=trust_civil_service,y=populist_attitudes_index_3dim))+
  geom_jitter()+
  geom_smooth()

pilot2 %>% 
  group_by(trust_civil_service) %>% 
  summarise(n=n(),mean(populist_attitudes_index_3dim))

# responses dataset

## responses dataset public service employees (1) # עובדים בשירות הציבורי
responses_public_service_employees <- pilot2 %>% 
  dplyr::select(response_id,
                pid,
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
  mutate(participant_response_id = str_c(pid,"_",response_order)) %>% 
  select(response_id,
         pid,
         civil_servant_label,
         response_order,
         participant_response_id,
         response,
         imagined_entity) %>% 
  filter(response!="")


## responses dataset  public service clerks (2) # פקידים בשירות הציבורי
responses_public_service_clerks <- pilot2 %>% 
  dplyr::select(response_id,
                pid,
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
  mutate(participant_response_id = str_c(pid,"_",response_order)) %>% 
  select(response_id,
         pid,
         civil_servant_label,
         response_order,
         participant_response_id,
         response,
         imagined_entity) %>% 
  filter(response!="")



## responses dataset bureaucrats (3) # בירוקרטים
responses_bureaucrats <- pilot2 %>% 
  dplyr::select(response_id,
                pid,
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
  mutate(participant_response_id = str_c(pid,"_",response_order)) %>% 
  select(response_id,
         pid,
         civil_servant_label,
         response_order,
         participant_response_id,
         response,
         imagined_entity) %>% 
  filter(response!="")



## responses dataset public service bureaucrats (4) # בירוקרטים בשירות הציבורי
responses_public_service_bureaucrats <- pilot2 %>% 
  dplyr::select(response_id,
                pid,
                a4_q119_1_text:a4_qid31,civil_servant_label) %>% 
  pivot_longer(cols = a4_q119_1_text:a4_q119_6_text,
               values_to = "response") %>% 
  mutate(response_order = name %>% 
           recode(
             `a4_q119_1_text` = 1,
             `a4_q119_2_text` = 2,
             `a4_q119_3_text` = 3,
             `a4_q119_4_text` = 4,
             `a4_q119_5_text` = 5,
             `a4_q119_6_text` = 6
           ),
         imagined_entity = a4_qid31) %>% 
  mutate(participant_response_id = str_c(pid,"_",response_order)) %>% 
  select(response_id,
         pid,
         civil_servant_label,
         response_order,
         participant_response_id,
         response,
         imagined_entity) %>% 
  filter(response!="")





responses_direction <- pilot2 %>% 
  dplyr::select(response_id,
                pid,
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
  mutate(participant_response_id = str_c(pid,"_",response_order)) %>% 
  select(response_id,
         pid,
         civil_servant_label,
         response_order,
         participant_response_id,
         response_direction) %>%
  drop_na(response_direction)


responses_direction_pilot2 <- bind_rows(responses_public_service_employees,
                                                responses_public_service_clerks,
                                                responses_bureaucrats,
                                                responses_public_service_bureaucrats) %>% 
  left_join(responses_direction)


#readr::write_excel_csv(responses_direction_pilot2,"responses_direction_pilot2_for_coding.csv")



