library(haven)
library(tidyverse)
library(janitor)


# Import data
wave1_data_raw <- read_sav("data/PopSter_wave_1.sav")


wave1 <- wave1_data_raw %>%
  janitor::clean_names() %>%
  filter(q38==8) %>%
  mutate(pid = 1:n())


## Demographics
wave1 <- wave1 %>% 
  mutate(duration_in_minutes = (duration_in_seconds/60) %>% round(1)) %>% 
  mutate(gender = qid6,
         education = qid8,
         age_group = qid7,
         sector = qid86,
         religiosity = qid87,
         district = qid88,
         income = qid89 %>% na_if(9),
         subjective_social_economic_status = qid90 %>% na_if(9),
         born_abroad = qid97,
         sector_employment = q205,
         public_sector_past = q206,
        emplyment_status = q45,
        general_trust = ifelse(q40==1,1,0),
  ) %>% 
  mutate(female = ifelse(gender==2,1,0)) %>% 
  mutate(public_sector_employee_current = ifelse(sector_employment==1, 1,0),
         public_sector_experience = ifelse(sector_employment==1 | public_sector_past==1, 1,0)) %>% 

# Political variables

mutate(external_efficecy = q194 %>% 
         dplyr::recode(`1`=4,
                       `2`=3,
                       `3`=2,
                       `4`=1) %>% labelled::remove_labels(), 
       right_left_ideology = dplyr::coalesce(q157) %>%  na_if(8),
       party_vote_past = dplyr::coalesce(q158),
       party_vote_intention = dplyr::coalesce(q159),
       political_block = dplyr::coalesce(q160),
       affect_right = dplyr::coalesce(q161_1),
       affect_left = dplyr::coalesce(q162_1)
) %>% 
  mutate(affective_polarization = abs(affect_right-affect_left)) %>% 
  mutate(coalition_party_vote_past = case_when(
    party_vote_past %in% c(1,3,5,6) ~ "Netanyahu coalition",
    party_vote_past %in% c(2,4,7,9,10,11) ~ "Anti Netanyahu coalition",
    TRUE ~ "Other"
  )) %>%
    mutate(coalition_party_vote_intention = case_when(
   party_vote_intention %in% c(1,3,5,6,13) ~ "Netanyahu coalition",
    party_vote_intention %in% c(2,4,7,8,9,10,11,14) ~ "Anti Netanyahu coalition",
    TRUE ~ "Other"
  )) %>%
  mutate(pro_netanyahu_coalition = 
    case_when(coalition_party_vote_intention=="Netanyahu coalition" ~ "Netanyahu coalition",
              coalition_party_vote_intention=="Other" & coalition_party_vote_past=="Netanyahu coalition" ~ "Netanyahu coalition",
              coalition_party_vote_intention=="Anti Netanyahu coalition" ~ "Anti-Netanyahu/Other",
              TRUE ~ "Anti-Netanyahu/Other"
    )) %>%

# Populist attitudes
  mutate(
    pop1 = q125_1,#popular sovereignty 1
    pop2 = q125_2,#popular sovereignty 2
    pop3 = q125_3,#popular sovereignty 3
    pop4 = q125_4,#anti elite 1
    pop5 = q125_5,  #anti elite 2
    pop6 = q125_6,#anti elite 3
    pop7 = q125_7,#anti elite 4
    pop8 = q125_8,#Homogeneity 1
    pop9 = q125_9,#Homogeneity 2
    pop10 = q125_10,#Homogeneity 3
    pop11 = q125_11,#Manichean 1
    pop12 = q125_12,#Manichean 2
    pop13 = q125_13#Manichean 3
  ) %>%
  mutate(
    across(
      .cols = pop1:pop13,
      .fns = ~ labelled::remove_labels(.x),
      .names = "{.col}_n"
    )
  )%>%
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::ends_with("_n"),
      .fns  = ~ dplyr::case_when(
        .x == 1 ~ 1,
        .x == 6 ~ 2,
        .x == 2 ~ 3,
        .x == 7 ~ 4,
        .x == 3 ~ 5,
        TRUE ~ NA_real_
      )
    )
  ) |> 

  mutate(popular_sovereignty = (((pop1_n + pop2_n + pop3_n)/3-1)/4),
         anti_elitism        = (((pop4_n + pop5_n + pop6_n + pop7_n)/4-1)/4),
         homogeneity         = (((pop8_n + pop9_n + pop10_n)/3-1)/4),
         manichean           = (((pop11_n + pop12_n + pop13_n)/3-1)/4)) %>% 
  mutate(populist_attitudes_index_4dim = (popular_sovereignty+anti_elitism+homogeneity+manichean)/4,
         populist_attitudes_index_3dim = (popular_sovereignty+anti_elitism+homogeneity)/3) %>% 
  mutate(
    populist_attitudes_index_4dim_multiplication = (popular_sovereignty*anti_elitism*homogeneity*manichean),
    populist_attitudes_index_4dim_minimum = pmin(scale(popular_sovereignty),scale(anti_elitism),scale(homogeneity),scale(manichean)) |> scale(),
    populist_attitudes_index_3dim_minimum = pmin(scale(popular_sovereignty),scale(anti_elitism),scale(homogeneity)) |> scale()
    ) |> 
  mutate(elite_group_text = stringr::str_c(a1_q127,a2_q127,a3_q127),
  elite_group_civil_servants = dplyr::case_when(
  a1_q31_8 == 1 ~ 1,
  a2_q31_8 == 1 ~ 1,
  a3_q31_8 == 1 ~ 1,
  TRUE ~ 0
)
) |> 
  mutate(civil_servants_label = dplyr::case_when(
  a1_q31_do_8 > 0 ~ "Public sector employees",
  a2_q31_do_8 > 0 ~ "Public sector bureaucrats",
  a3_q31_do_8 > 0 ~ "Public sector clerks"
)) |> 
  
  
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
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::starts_with("trust_"),
      .fns  = ~ dplyr::case_when(
        .x == 1 ~ 4,
        .x == 2 ~ 3,
        .x == 3 ~ 2,
        .x == 4 ~ 1,
        TRUE ~ NA_real_
      )
    )
  )%>% 


# Conspiracy theory

  mutate(conspiracy_deep_state = q232,
         conspiracy_deep_state_n = q232 %>% na_if(6),
         conspiracy_vaccine = q235,
         conspiracy_vaccine_n = q235 %>% na_if(6))  |> 
  mutate(conspiracy_deep_state_n = (conspiracy_deep_state_n-1)/4,
         conspiracy_vaccine_n = (conspiracy_vaccine_n-1)/4
         ) |>       
  
  
  
# Attraction to work in public sector

mutate(
  attraction_public_sector_item1 = q45_1,
  attraction_public_sector_item2 = q45_2,
  attraction_public_sector_item3 = q45_3
) |> 
mutate(
  attraction_public_sector = 
    ((attraction_public_sector_item1 + attraction_public_sector_item2 + attraction_public_sector_item3-3)/12)
) |> 


  mutate(psm_aps_1 = q43_1,
         psm_aps_2 = q43_2,
         psm_aps_3 = q43_3,
         psm_aps_4 = q43_4
        ) |> 
mutate(
  psm_aps = 
    ((psm_aps_1 + psm_aps_2 + psm_aps_3 + psm_aps_4-4)/16)
)

save.image()