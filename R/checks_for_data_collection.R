###############################################################################

# checks for data collection
# read packages
library(tidyverse)
library(lubridate)
library(glue)
library(supporteR)

source("R/composite_indicators.R")

# read data and tool ----------------------------------------------------------
# data
data_path <- "inputs/ETH2403_MSNA_2024_data.xlsx"

df_tool_data <- readxl::read_excel(data_path) |>  
  mutate(start = as_datetime(start),
         end = as_datetime(end),
         enumerator_id = ifelse(is.na(enum_id), enum_id, enum_id)) |> 
  checks_add_extra_cols(input_enumerator_id_col = "enumerator_id",
                        input_location_col = "admin4") |> 
  
  create_composite_indicators() |> 
  rowwise() |> 
  mutate(i.hh_size = case_when(hh_size <= 3 ~ "between_1_and_3_members",
                               hh_size <= 6 ~ "between_4_and_6_members",
                               hh_size <= 9 ~ "between_7_and_9_members",
                               hh_size >= 10 ~ "10_or_more_members"),
         i.respondent_age = case_when(resp_age <= 24 ~ "age_18_24",
                                 resp_age <= 39 ~ "age_25_39",
                                 resp_age <= 59 ~ "age_40_59",
                                 resp_age > 59 ~ "age_60+"),
    #i.hh_tot_income = sum(c_across(last30_income_agriculture_livestock:last30_hh_other_income_amount), na.rm = T),
    #i.tot_expenditure = sum(c_across(expenditure_food:expenditure_other_frequent), na.rm = T)
  ) |>
  ungroup()

# loops -----------------------------------------------------------------------
# loop_roster
loop_roster <- readxl::read_excel(path = data_path, sheet = "roster")

df_raw_data_loop_roster <- df_tool_data |> 
  select(-`_index`) |> 
  inner_join(loop_roster, by = c("_uuid" = "_submission__uuid"))

# loop_health_ind
loop_health_ind <- readxl::read_excel(path = data_path, sheet = "health_ind")

df_raw_data_loop_health_ind <- df_tool_data |> 
  select(-`_index`) |> 
  inner_join(loop_health_ind, by = c("_uuid" = "_submission__uuid"))

# loop_education_ind
loop_edu_ind <- readxl::read_excel(path = data_path, sheet = "edu_ind")

df_raw_data_loop_edu_ind <- df_tool_data |> 
  select(-`_index`) |> 
  inner_join(loop_edu_ind, by = c("_uuid" = "_submission__uuid"))

# tool

loc_tool <- "inputs/ETH2403_MSNA_2024_tool.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey") 
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

# checks ----------------------------------------------------------------------

checks_output <- list()

# testing data ----------------------------------------------------------------

df_testing_data <- df_tool_data |> 
  filter(i.check.start_date < as_date("2024-05-24")) |> 
  mutate(i.check.type = "remove_survey",
         i.check.name = "",
         i.check.current_value = "",
         i.check.value = "",
         i.check.issue_id = "logic_c_testing_data",
         i.check.issue = "testing_data",
         i.check.other_text = "",
         i.check.checked_by = "GG",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  dplyr::select(starts_with("i.check.")) |> 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = "")) 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_testing_data")

# no consent --------------------------------------------------------------

df_no_consent <- df_tool_data |> 
  filter(consent == "no") |> 
  mutate(i.check.type = "remove_survey",
         i.check.name = "consent",
         i.check.current_value = consent,
         i.check.value = "",
         i.check.issue_id = "logic_c_requirement_no_consent",
         i.check.issue = "no_consent",
         i.check.other_text = "",
         i.check.checked_by = "GG",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |>  
  dplyr::select(starts_with("i.check")) |> 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = "")) 

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_no_consent")

# fix enumerator_id data ------------------------------------------------------

df_logic_c_enumerator_id_harmonization <- df_tool_data |> 
  filter(is.na(enumerator_id), i.check.start_date > as_date("2024-05-24")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "enumerator_id",
         i.check.current_value = "NA",
         i.check.value = enumerator_id,
         i.check.issue_id = "logic_c_enumerator_id_harmonization",
         i.check.issue = "enumerator_id_harmonization",
         i.check.other_text = "",
         i.check.checked_by = "GG",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_enumerator_id_harmonization")

# Time checks -----------------------------------------------------------------
# Time interval for the survey
min_time_of_survey <- 25
max_time_of_survey <- 120

df_c_survey_time <-  supporteR::check_survey_time(input_tool_data = df_tool_data, 
                                                  input_enumerator_id_col = "enumerator_id",
                                                  input_location_col = "admin4",
                                                  input_min_time = min_time_of_survey, 
                                                  input_max_time = max_time_of_survey) 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_survey_time")

# check duplicate uuids -------------------------------------------------------

df_c_duplicate_uuid <-  supporteR::checks_duplicate_uuids(input_tool_data = df_tool_data) 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_duplicate_uuid")

# outliers --------------------------------------------------------------------

df_c_outliers <- supporteR::check_outliers_cleaninginspector(input_tool_data = df_tool_data,
                                                             input_enumerator_id_col = "enumerator_id",
                                                             input_location_col = "admin4") 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_c_outliers")

# other_specify ---------------------------------------------------------------

df_others_data <- supporteR::extract_other_specify_data(input_tool_data = df_tool_data |> select(-snfi_shelter_damagel_other), 
                                                        input_enumerator_id_col = "enumerator_id",
                                                        input_location_col = "admin4",
                                                        input_survey = df_survey,  
                                                        input_choices = df_choices) 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_others_data")

# repeat_other_specify --------------------------------------------------------

df_repeat_others_data <- supporteR::extract_other_specify_data_repeats(input_repeat_data = df_raw_data_loop_health_ind |> mutate(`_index.y` = `_index`), 
                                                                       input_enumerator_id_col = "enumerator_id", 
                                                                       input_location_col = "admin4", 
                                                                       input_survey = df_survey, 
                                                                       input_choices = df_choices, 
                                                                       input_sheet_name = "health_ind", 
                                                                       input_repeat_cols = c("health_ind_healthcare_needed_type", "health_healthcare_paying_service",
                                                                                             "health_barriers_unmet", "health_barriers_met", "health_barriers_no_need")) 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_repeat_others_data")

# logical checks --------------------------------------------------------------
# If "hh_size" = 1 and response to relation to household head "relation_to_hoh" is not "spouse_hohh"
df_relation_to_hoh <- df_tool_data %>% 
  filter(!hoh_relationship %in% c("spouse_hohh") , hh_size == 1) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "hoh_relationship",
         i.check.current_value = hoh_relationship,
         i.check.value = "",
         i.check.issue_id = "logic_c_relation_to_hoh_mismatch",
         i.check.issue = glue("relation_to_hoh: {hoh_relationship}, but hh_size is: {hh_size}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "Response to change to 'spouse_hohh' since respondent lives alone", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "logic_output", input_df_name = "df_relation_to_hoh")

# Household size seems to be unusually low (below 2) or high (above 8); survey needs to be checked
df_logic_c_hh_size_seems_unusal <- df_tool_data |> 
  filter(hh_size <= 2 | hh_size > 8) |> 
  mutate(i.check.type = "remove_survey",
         i.check.name = "hh_size",
         i.check.current_value = as.character(hh_size),
         i.check.value = "",
         i.check.issue_id = "hh_size_seems_unusal",
         i.check.issue = "household size seems unusal",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "care to be taken in deciding how to use this data", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  dplyr::select(starts_with("i.check.")) |> 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = "")) 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_size_seems_unusal")

# The age of the hoh seems too high (80 years old or higher), please check the age of the hoh again.
df_logic_c_hoh_age_seems_too_high <- df_tool_data |> 
  filter(resp_hoh_yn %in% c("no"), hoh_age >= 80) |>
  mutate(i.check.type = "change_response",
         i.check.name = "resp_hoh_yn",
         i.check.current_value = resp_hoh_yn,
         i.check.value = "",
         i.check.issue_id = "hoh_age_seems_too_high",
         i.check.issue = glue("hoh: {resp_hoh_yn} but hoh_age:{hoh_age}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename() 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hoh_age_seems_too_high")

# HH reports 'sell more livestock than usual', but reports not owning any livestock
df_logic_c_sell_livestock_but_not_owning_any_livestock <- df_tool_data |> 
  filter(fsl_lcsi_en_crisis1 %in%  c("yes"), cm_assets_ownership_agriculture %in% c( "no")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_lcsi_en_crisis1",
         i.check.current_value = fsl_lcsi_en_crisis1,
         i.check.value = "",
         i.check.issue_id = "hh_response_sell_livestock_but_not_owning_any_livestock",
         i.check.issue = glue("lcsi_crisis: {fsl_lcsi_en_crisis1} but hh_not_own_livestock: {cm_assets_ownership_agriculture}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename() 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_sell_livestock_but_not_owning_any_livestock")

# HH reports 'sell the last female animal', but reports not owning any livestock
df_logic_c_sell_female_animal_but_not_owning_any_livestock <- df_tool_data |> 
  filter(fsl_lcsi_en_emergency1 %in%  c("yes"), cm_assets_ownership_agriculture %in% c( "no")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_lcsi_en_emergency1",
         i.check.current_value = fsl_lcsi_en_emergency1,
         i.check.value = "",
         i.check.issue_id = "hh_response_sell_female_animal_but_not_owning_any_livestock",
         i.check.issue = glue("fsl_lcsi_en_emergency1: {fsl_lcsi_en_emergency1} but hh_not_own_livestock: {cm_assets_ownership_agriculture}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename() 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_sell_female_animal_but_not_owning_any_livestock")

# If fsl_fcs_condiments = 0 i.e. household has not eaten salt, spices, tea, or coffee in the past seven days, surveys should be checked
df_logic_c_hh_no_eating_condiments <- df_tool_data %>% 
  filter(fsl_fcs_condiments == 0) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_fcs_condiments",
         i.check.current_value = as.character(fsl_fcs_condiments),
         i.check.value = "NA",
         i.check.issue_id = "hh_no_eating_condiments",
         i.check.issue = glue("fsl_fcs_condiments: {fsl_fcs_condiments}, it's unlikely that a household spent 7 days eating food without salt"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "enumerators misinterpreted question", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_no_eating_condiments")

# If fsl_fcs_legumes = 0 i.e. household has not eaten any beans/legumes, pulses or nuts in the past seven days, surveys should be checked
df_logic_c_hh_no_eating_beans_nuts <- df_tool_data %>% 
  filter(fsl_fcs_legumes == 0) %>%
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_fcs_legumes",
         i.check.current_value = as.character(fsl_fcs_legumes),
         i.check.value = "NA",
         i.check.issue_id = "hh_no_eating_beans_nuts",
         i.check.issue = glue("fsl_fcs_legumes: {fsl_fcs_legumes}, it's unlikely that a household spent 7 days eating food without any beans/legumes, pulses or nuts"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "enumerators misinterpreted question", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check")) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_no_eating_beans_nuts")

# same value of fcs components
df_fd_consumption_score_same <- df_tool_data |>  
  filter(if_all(c(fsl_fcs_cereal, fsl_fcs_legumes, fsl_fcs_veg, fsl_fcs_fruit, fsl_fcs_condiments, 
                  fsl_fcs_meat, fsl_fcs_dairy, fsl_fcs_sugar, fsl_fcs_oil), ~ fsl_fcs_cereal == .x))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_fcs_cereal",
         i.check.current_value = as.character(fsl_fcs_cereal),
         i.check.value = "",
         i.check.issue_id = "logic_c_fd_consumption_score_same",
         i.check.issue = glue("fsl_fcs_cereal:{fsl_fcs_cereal}, fsl_fcs_legumes:{fsl_fcs_legumes}, fsl_fcs_veg:{fsl_fcs_veg}, fsl_fcs_fruit:{fsl_fcs_fruit}, fsl_fcs_condiments:{fsl_fcs_condiments}, fsl_fcs_meat:{fsl_fcs_meat}, fsl_fcs_dairy:{fsl_fcs_dairy}, fsl_fcs_sugar:{fsl_fcs_sugar}, fsl_fcs_oil:{fsl_fcs_oil}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  slice(rep(1:n(), each = 9)) |>  
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fsl_fcs_cereal", 
                                  rank == 2 ~ "fsl_fcs_legumes",
                                  rank == 3 ~ "fsl_fcs_veg", 
                                  rank == 4 ~ "fsl_fcs_fruit", 
                                  rank == 5 ~ "fsl_fcs_condiments", 
                                  rank == 6 ~ "fsl_fcs_meat", 
                                  rank == 7 ~ "fsl_fcs_dairy", 
                                  rank == 8 ~ "fsl_fcs_sugar", 
                                  TRUE ~ "fsl_fcs_oil"),
         i.check.current_value = case_when(rank == 1 ~ as.character(fsl_fcs_cereal),
                                           rank == 2 ~ as.character(fsl_fcs_legumes),
                                           rank == 3 ~ as.character(fsl_fcs_veg), 
                                           rank == 4 ~ as.character(fsl_fcs_fruit), 
                                           rank == 5 ~ as.character(fsl_fcs_condiments), 
                                           rank == 6 ~ as.character(fsl_fcs_meat), 
                                           rank == 7 ~ as.character(fsl_fcs_dairy), 
                                           rank == 8 ~ as.character(fsl_fcs_sugar), 
                                           TRUE ~ as.character(fsl_fcs_oil))
  ) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_fd_consumption_score_same")

# same value of rcsi components and more than 0 times of rcsi. suspect that enumerators may have just filled
df_fd_rcsi_same <- df_tool_data |>  
  filter(if_all(c(fsl_rcsi_lessquality, fsl_rcsi_mealsize, fsl_rcsi_mealadult, fsl_rcsi_mealnb,
                  fsl_rcsi_borrow), ~ fsl_rcsi_lessquality == .x & fsl_rcsi_lessquality > 0))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_rcsi_lessquality",
         i.check.current_value = as.character(fsl_rcsi_lessquality),
         i.check.value = "",
         i.check.issue_id = "logic_c_fd_rcsi_same",
         i.check.issue = glue("fsl_rcsi_lessquality :{fsl_rcsi_lessquality}, fsl_rcsi_mealsize :{fsl_rcsi_mealsize}, fsl_rcsi_mealadult :{fsl_rcsi_mealadult}, fsl_rcsi_mealnb :{fsl_rcsi_mealnb}, fsl_rcsi_borrow :{fsl_rcsi_borrow}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  slice(rep(1:n(), each = 5)) |>  
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fsl_rcsi_lessquality", 
                                  rank == 2 ~ "fsl_rcsi_mealsize",
                                  rank == 3 ~ "fsl_rcsi_mealadult", 
                                  rank == 4 ~ "fsl_rcsi_mealnb", 
                                  TRUE ~ "fsl_rcsi_borrow"),
         i.check.current_value = case_when(rank == 1 ~ as.character(fsl_rcsi_lessquality),
                                           rank == 2 ~ as.character(fsl_rcsi_mealsize),
                                           rank == 3 ~ as.character(fsl_rcsi_mealadult), 
                                           rank == 4 ~ as.character(fsl_rcsi_mealnb), 
                                           TRUE ~ as.character(fsl_rcsi_borrow))
  ) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_fd_rcsi_same")

# HH take short time to access the nearest health facility but report barriers:"Health facility is too far away"
df_logic_c_hh_report_short_time_but_health_facility_far_away <- df_raw_data_loop_health_ind |> 
  filter(health_barriers_unmet %in% c("health_fac_far"), health_facility_time < 30) |>
  mutate(i.check.type = "change_response",
         i.check.name = "health_barriers_unmet",
         i.check.current_value = health_barriers_unmet,
         i.check.value = "",
         i.check.issue_id = "hh_report_short_time_but_health_facility_far_away",
         i.check.issue = glue("healthcare barriers: {health_barriers_unmet} but health_facility_distance: {health_facility_time}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  batch_select_rename()

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_report_short_time_but_health_facility_far_away")

# check FSL -------------------------------------------------------------------
# check_FCS_high_HHS_high
# HH has a good diet score: > 38.5 but it was also reported that the household had no food: "yes"
df_logic_c_hh_has_good_diet_but_nofood <- df_tool_data |> 
  filter(fsl_hhs_nofoodh %in% c("yes"),
         fsl_hhs_sleephungry %in% c("yes"),
         fsl_hhs_alldaynight %in% c("yes"), i.fcs >= 38.5) |>
  mutate(i.check.type = "change_response",
         i.check.name = " fsl_hhs_nofoodh",
         i.check.current_value = as.character(fsl_hhs_nofoodh),
         i.check.value = "",
         i.check.issue_id = "hh_has_good_diet_but_nofood",
         i.check.issue = glue("fsl_hhs_nofoodh :{fsl_hhs_nofoodh}, fsl_hhs_sleephungry :{fsl_hhs_sleephungry}, fsl_hhs_alldaynight :{fsl_hhs_alldaynight}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  slice(rep(1:n(), each = 3)) |> 
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |> 
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fsl_hhs_nofoodh",
                                  rank == 2 ~ "fsl_hhs_sleephungry",
                                  TRUE ~ "fsl_hhs_alldaynight"),
         i.check.current_value = case_when(rank == 1 ~ as.character("fsl_hhs_nofoodh"),
                                           rank == 2 ~ as.character("fsl_hhs_sleephungry"),
                                           TRUE ~ as.character("fsl_hhs_alldaynight")) )|> 
  dplyr::select(starts_with("i.check.")) |>
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))|> 
  filter(!is.na(current_value))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_has_good_diet_but_nofood")

# check_FCS_low_HHS_low
# HH has an alarming diet score: < 38.5 but it was also reported that the household had no food: "no"
df_logic_c_hh_has_alarming_diet_but_nofood <- df_tool_data |>
  filter(fsl_hhs_nofoodh %in% c("no"),
         fsl_hhs_sleephungry %in% c("no"),
         fsl_hhs_alldaynight %in% c("no"), i.fcs <= 38.5) |>
  mutate(i.check.type = "change_response",
         i.check.name = " fsl_hhs_nofoodh",
         i.check.current_value = as.character(fsl_hhs_nofoodh),
         i.check.value = "",
         i.check.issue_id = "hh_has_alarming_diet_but_nofood",
         i.check.issue = glue("fsl_hhs_nofoodh :{fsl_hhs_nofoodh}, fsl_hhs_sleephungry :{fsl_hhs_sleephungry}, fsl_hhs_alldaynight :{fsl_hhs_alldaynight}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  slice(rep(1:n(), each = 3)) |> 
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |> 
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fsl_hhs_nofoodh",
                                  rank == 2 ~ "fsl_hhs_sleephungry",
                                  TRUE ~ "fsl_hhs_alldaynight"),
         i.check.current_value = case_when(rank == 1 ~ as.character("fsl_hhs_nofoodh"),
                                           rank == 2 ~ as.character("fsl_hhs_sleephungry"),
                                           TRUE ~ as.character("fsl_hhs_alldaynight")) )|> 
  dplyr::select(starts_with("i.check.")) |>
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))|> 
  filter(!is.na(current_value))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_has_alarming_diet_but_nofood")

#check_FCS_high_LCSI_high
# It was reported that the the household has a good diet score but reports stress, crisis or emergency strategies for accessing food
df_logic_c_hh_has_good_diet_score_but_stress_crisis_emergency <- df_tool_data |>
  filter(!fsl_lcsi_en_stress1 %in% c("no_had_no_need"),
         !fsl_lcsi_en_stress2 %in% c("no_had_no_need"),
         !fsl_lcsi_en_stress3 %in% c("no_had_no_need"),
         !fsl_lcsi_en_stress4 %in% c("no_had_no_need"),
         !fsl_lcsi_en_crisis1 %in% c("no_had_no_need"),
         !fsl_lcsi_en_crisis2 %in% c("no_had_no_need"),
         !fsl_lcsi_en_crisis3 %in% c("no_had_no_need"),
         !fsl_lcsi_en_emergency1 %in% c("no_had_no_need"),
         !fsl_lcsi_en_emergency2 %in% c("no_had_no_need"),
         !fsl_lcsi_en_emergency3 %in% c("no_had_no_need"), i.fcs >= 38.5) |>
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_lcsi_en_stress1",
         i.check.current_value = as.character(fsl_lcsi_en_stress1),
         i.check.value = "",
         i.check.issue_id = "hh_has_good_diet_score_but_stress_crisis_emergency",
         i.check.issue = glue("fsl_lcsi_en_stress1 :{fsl_lcsi_en_stress1}, fsl_lcsi_en_stress2 :{fsl_lcsi_en_stress2}, fsl_lcsi_en_stress3 :{fsl_lcsi_en_stress3}, fsl_lcsi_en_stress4 :{fsl_lcsi_en_stress4}, fsl_lcsi_en_crisis1 :{fsl_lcsi_en_crisis1}, fsl_lcsi_en_crisis2 :{fsl_lcsi_en_crisis2}, fsl_lcsi_en_crisis3 :{fsl_lcsi_en_crisis3}, fsl_lcsi_en_emergency1 :{fsl_lcsi_en_emergency1}, fsl_lcsi_en_emergency2 :{fsl_lcsi_en_emergency2}, fsl_lcsi_en_emergency3 :{fsl_lcsi_en_emergency3}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  slice(rep(1:n(), each = 10)) |> 
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |> 
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fsl_lcsi_en_stress1", 
                                  rank == 2 ~ "fsl_lcsi_en_stress2",
                                  rank == 3 ~ "fsl_lcsi_en_stress3", 
                                  rank == 4 ~ "fsl_lcsi_en_stress4", 
                                  rank == 5 ~ "fsl_lcsi_en_crisis1", 
                                  rank == 6 ~ "fsl_lcsi_en_crisis2", 
                                  rank == 7 ~ "fsl_lcsi_en_crisis3", 
                                  rank == 8 ~ "fsl_lcsi_en_emergency1", 
                                  rank == 9 ~ "fsl_lcsi_en_emergency2", 
                                  TRUE ~ "fsl_lcsi_en_emergency3"),
         i.check.current_value = case_when(rank == 1 ~ as.character(fsl_lcsi_en_stress1),
                                           rank == 2 ~ as.character(fsl_lcsi_en_stress2),
                                           rank == 3 ~ as.character(fsl_lcsi_en_stress3), 
                                           rank == 4 ~ as.character(fsl_lcsi_en_stress4), 
                                           rank == 5 ~ as.character(fsl_lcsi_en_crisis1), 
                                           rank == 6 ~ as.character(fsl_lcsi_en_crisis2), 
                                           rank == 7 ~ as.character(fsl_lcsi_en_crisis3), 
                                           rank == 8 ~ as.character(fsl_lcsi_en_emergency1), 
                                           rank == 9 ~ as.character(fsl_lcsi_en_emergency2), 
                                           TRUE ~ as.character(fsl_lcsi_en_emergency3)) 
  )|> 
  dplyr::select(starts_with("i.check.")) |>
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))|> 
  filter(!is.na(current_value))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_has_good_diet_score_but_stress_crisis_emergency")

#check_FCS_low_LCSI_low
# It was reported that the household has an alarming diet score but does not report stress, crisis or emergency strategies
df_logic_c_hh_has_alarming_diet_score_but_stress_crisis_emergency <- df_tool_data |>
  filter(fsl_lcsi_en_stress1 %in% c("no_had_no_need"),
         fsl_lcsi_en_stress2 %in% c("no_had_no_need"),
         fsl_lcsi_en_stress3 %in% c("no_had_no_need"),
         fsl_lcsi_en_stress4 %in% c("no_had_no_need"),
         fsl_lcsi_en_crisis1 %in% c("no_had_no_need"),
         fsl_lcsi_en_crisis2 %in% c("no_had_no_need"),
         fsl_lcsi_en_crisis3 %in% c("no_had_no_need"),
         fsl_lcsi_en_emergency1 %in% c("no_had_no_need"),
         fsl_lcsi_en_emergency2 %in% c("no_had_no_need"),
         fsl_lcsi_en_emergency3 %in% c("no_had_no_need"), i.fcs <= 38.5) |>
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_lcsi_en_stress1",
         i.check.current_value = as.character(fsl_lcsi_en_stress1),
         i.check.value = "",
         i.check.issue_id = "hh_has_alarming_diet_score_but_stress_crisis_emergency",
         i.check.issue = glue("fsl_lcsi_en_stress1 :{fsl_lcsi_en_stress1}, fsl_lcsi_en_stress2 :{fsl_lcsi_en_stress2}, fsl_lcsi_en_stress3 :{fsl_lcsi_en_stress3}, fsl_lcsi_en_stress4 :{fsl_lcsi_en_stress4}, fsl_lcsi_en_crisis1 :{fsl_lcsi_en_crisis1}, fsl_lcsi_en_crisis2 :{fsl_lcsi_en_crisis2}, fsl_lcsi_en_crisis3 :{fsl_lcsi_en_crisis3}, fsl_lcsi_en_emergency1 :{fsl_lcsi_en_emergency1}, fsl_lcsi_en_emergency2 :{fsl_lcsi_en_emergency2}, fsl_lcsi_en_emergency3 :{fsl_lcsi_en_emergency3}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  slice(rep(1:n(), each = 10)) |> 
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |> 
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fsl_lcsi_en_stress1", 
                                  rank == 2 ~ "fsl_lcsi_en_stress2",
                                  rank == 3 ~ "fsl_lcsi_en_stress3", 
                                  rank == 4 ~ "fsl_lcsi_en_stress4", 
                                  rank == 5 ~ "fsl_lcsi_en_crisis1", 
                                  rank == 6 ~ "fsl_lcsi_en_crisis2", 
                                  rank == 7 ~ "fsl_lcsi_en_crisis3", 
                                  rank == 8 ~ "fsl_lcsi_en_emergency1", 
                                  rank == 9 ~ "fsl_lcsi_en_emergency2", 
                                  TRUE ~ "fsl_lcsi_en_emergency3"),
         i.check.current_value = case_when(rank == 1 ~ as.character(fsl_lcsi_en_stress1),
                                           rank == 2 ~ as.character(fsl_lcsi_en_stress2),
                                           rank == 3 ~ as.character(fsl_lcsi_en_stress3), 
                                           rank == 4 ~ as.character(fsl_lcsi_en_stress4), 
                                           rank == 5 ~ as.character(fsl_lcsi_en_crisis1), 
                                           rank == 6 ~ as.character(fsl_lcsi_en_crisis2), 
                                           rank == 7 ~ as.character(fsl_lcsi_en_crisis3), 
                                           rank == 8 ~ as.character(fsl_lcsi_en_emergency1), 
                                           rank == 9 ~ as.character(fsl_lcsi_en_emergency2), 
                                           TRUE ~ as.character(fsl_lcsi_en_emergency3)) 
  )|> 
  dplyr::select(starts_with("i.check.")) |>
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))|> 
  filter(!is.na(current_value))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_has_alarming_diet_score_but_stress_crisis_emergency")

#High FCS score and rcsi used
# The household diet score is reported to be good but the household reports using reduced coping strategies.
df_logic_c_hh_has_good_diet_score_but_using_coping_strategies <- df_tool_data |>
  filter(fsl_rcsi_lessquality > 0,
         fsl_rcsi_mealsize > 0, 
         fsl_rcsi_mealadult > 0,
         fsl_rcsi_mealnb > 0,
         fsl_rcsi_borrow > 0, i.fcs >= 38.5) |>
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_rcsi_lessquality",
         i.check.current_value = as.character(fsl_rcsi_lessquality),
         i.check.value = "",
         i.check.issue_id = "hh_has_good_diet_score_but_using_coping_strategies",
         i.check.issue = glue("fsl_rcsi_lessquality :{fsl_rcsi_lessquality}, fsl_rcsi_mealsize :{fsl_rcsi_mealsize}, fsl_rcsi_mealadult :{fsl_rcsi_mealadult}, fsl_rcsi_mealnb :{fsl_rcsi_mealnb}, fsl_rcsi_borrow :{fsl_rcsi_borrow}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  slice(rep(1:n(), each = 5)) |> 
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |> 
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fsl_rcsi_lessquality", 
                                  rank == 2 ~ "fsl_rcsi_mealsize", 
                                  rank == 3 ~ "fsl_rcsi_mealadult", 
                                  rank == 4 ~ "fsl_rcsi_mealnb", 
                                  TRUE ~ "fsl_rcsi_borrow"),
         i.check.current_value = case_when(rank == 1 ~ as.character(fsl_rcsi_lessquality), 
                                           rank == 2 ~ as.character(fsl_rcsi_mealsize), 
                                           rank == 3 ~ as.character(fsl_rcsi_mealadult), 
                                           rank == 4 ~ as.character(fsl_rcsi_mealnb), 
                                           TRUE ~ as.character(fsl_rcsi_borrow)) 
  )|> 
  dplyr::select(starts_with("i.check.")) |>
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))|> 
  filter(!is.na(current_value))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_has_good_diet_score_but_using_coping_strategies")

#Low FCS score but rcsi not used
# The household diet score is reported to be alarming but the household does not report using reduced coping strategies.
df_logic_c_hh_has_alarming_diet_score_but_no_coping_strategies <- df_tool_data |>
  filter(fsl_rcsi_lessquality == 0,
         fsl_rcsi_mealsize == 0, 
         fsl_rcsi_mealadult == 0,
         fsl_rcsi_mealnb == 0,
         fsl_rcsi_borrow == 0, i.fcs <= 38.5) |>
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_rcsi_lessquality",
         i.check.current_value = as.character(fsl_rcsi_lessquality),
         i.check.value = "",
         i.check.issue_id = "hh_has_alarming_diet_score_but_no_coping_strategies",
         i.check.issue = glue("fsl_rcsi_lessquality :{fsl_rcsi_lessquality}, fsl_rcsi_mealsize :{fsl_rcsi_mealsize}, fsl_rcsi_mealadult :{fsl_rcsi_mealadult}, fsl_rcsi_mealnb :{fsl_rcsi_mealnb}, fsl_rcsi_borrow :{fsl_rcsi_borrow}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "")  |> 
  slice(rep(1:n(), each = 5)) |> 
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |> 
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fsl_rcsi_lessquality", 
                                  rank == 2 ~ "fsl_rcsi_mealsize", 
                                  rank == 3 ~ "fsl_rcsi_mealadult", 
                                  rank == 4 ~ "fsl_rcsi_mealnb", 
                                  TRUE ~ "fsl_rcsi_borrow"),
         i.check.current_value = case_when(rank == 1 ~ as.character(fsl_rcsi_lessquality), 
                                           rank == 2 ~ as.character(fsl_rcsi_mealsize), 
                                           rank == 3 ~ as.character(fsl_rcsi_mealadult), 
                                           rank == 4 ~ as.character(fsl_rcsi_mealnb), 
                                           TRUE ~ as.character(fsl_rcsi_borrow)) 
  )|> 
  dplyr::select(starts_with("i.check.")) |>
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))|> 
  filter(!is.na(current_value))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_hh_has_alarming_diet_score_but_no_coping_strategies")

# mismatch between FCS and HHS
df_logic_c_fcs_and_hhs_mismatch <- df_tool_data |> 
  filter((i.fcs_cat %in% c("Acceptable")), i.hhs_cat %in% c("Severe")) |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_fcs_cereal",
         i.check.current_value = as.character(fsl_fcs_cereal),
         i.check.value = "",
         i.check.issue_id = "logic_c_mismatch_btn_fcs_and_hhs",
         i.check.issue = "mismatch between FCS and HHS",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "",
         i.check.reviewed = "1",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |>
  slice(rep(1:n(), each = 15)) |> 
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |> 
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fsl_fcs_cereal", 
                                  rank == 2 ~ "fsl_fcs_legumes",
                                  rank == 3 ~ "fsl_fcs_veg", 
                                  rank == 4 ~ "fsl_fcs_fruit", 
                                  rank == 5 ~ "fsl_fcs_condiments", 
                                  rank == 6 ~ "fsl_fcs_meat", 
                                  rank == 7 ~ "fsl_fcs_dairy", 
                                  rank == 8 ~ "fsl_fcs_sugar", 
                                  rank == 9 ~ "fsl_fcs_oil",
                                  rank == 10 ~ "fsl_hhs_nofoodh",
                                  rank == 11 ~ "fsl_hhs_nofoodhh_freq",
                                  rank == 12 ~ "fsl_hhs_sleephungry",
                                  rank == 13 ~ "fsl_hhs_sleephungry_freq",
                                  rank == 14 ~ "fsl_hhs_alldaynight",
                                  TRUE ~ "fsl_hhs_alldaynight_freq"),
         i.check.current_value = case_when(rank == 1 ~ as.character(fsl_fcs_cereal),
                                           rank == 2 ~ as.character(fsl_fcs_legumes),
                                           rank == 3 ~ as.character(fsl_fcs_veg), 
                                           rank == 4 ~ as.character(fsl_fcs_fruit), 
                                           rank == 5 ~ as.character(fsl_fcs_condiments), 
                                           rank == 6 ~ as.character(fsl_fcs_meat), 
                                           rank == 7 ~ as.character(fsl_fcs_dairy), 
                                           rank == 8 ~ as.character(fsl_fcs_sugar), 
                                           rank == 9 ~ as.character(fsl_fcs_oil), 
                                           rank == 10 ~ as.character(fsl_hhs_nofoodh), 
                                           rank == 11 ~ as.character(fsl_hhs_nofoodhh_freq), 
                                           rank == 12 ~ as.character(fsl_hhs_sleephungry), 
                                           rank == 13 ~ as.character(fsl_hhs_sleephungry_freq), 
                                           rank == 14 ~ as.character(fsl_hhs_alldaynight), 
                                           TRUE ~ as.character(fsl_hhs_alldaynight_freq))
  ) |> 
  dplyr::select(starts_with("i.check.")) |>
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = "")) |> 
  filter(!is.na(current_value))
  
add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_fcs_and_hhs_mismatch")

# - i.hhs_cat[severe] // i.rcsi_cat[0] *** take out all food indicators ***
df_logic_c_fd_hhs_severe_but_rcsi_low <- df_tool_data |>  
  filter(i.hhs_cat %in% c("Severe"), i.rcsi_cat %in% c("rcsi_0_3"))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_fcs_cereal",
         i.check.current_value = as.character(fsl_fcs_cereal),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_fd_hhs_severe_but_rcsi_low",
         i.check.issue = glue("hhs_severe_but_rcsi_low"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  slice(rep(1:n(), each = 20)) |>  
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fsl_fcs_cereal", 
                                  rank == 2 ~ "fsl_fcs_legumes",
                                  rank == 3 ~ "fsl_fcs_veg", 
                                  rank == 4 ~ "fsl_fcs_fruit", 
                                  rank == 5 ~ "fsl_fcs_condiments", 
                                  rank == 6 ~ "fsl_fcs_meat", 
                                  rank == 7 ~ "fsl_fcs_dairy", 
                                  rank == 8 ~ "fsl_fcs_sugar", 
                                  rank == 9 ~ "fsl_fcs_oil", 
                                  rank == 10 ~ "fsl_hhs_nofoodh", 
                                  rank == 11 ~ "fsl_hhs_nofoodhh_freq", 
                                  rank == 12 ~ "fsl_hhs_sleephungry", 
                                  rank == 13 ~ "fsl_hhs_sleephungry_freq", 
                                  rank == 14 ~ "fsl_hhs_alldaynight", 
                                  rank == 15 ~ "fsl_hhs_alldaynight_freq", 
                                  rank == 16 ~ "fsl_rcsi_lessquality", 
                                  rank == 17 ~ "fsl_rcsi_mealsize", 
                                  rank == 18 ~ "fsl_rcsi_mealadult", 
                                  rank == 19 ~ "fsl_rcsi_mealnb", 
                                  TRUE ~ "fsl_rcsi_borrow"),
         i.check.value = case_when(i.check.name %in% c("fsl_hhs_nofoodh", 
                                                       "fsl_hhs_sleephungry",
                                                       "fsl_hhs_alldaynight") ~ "0", 
                                   TRUE ~ i.check.value),
         i.check.current_value = case_when(rank == 1 ~ as.character(fsl_fcs_cereal),
                                           rank == 2 ~ as.character(fsl_fcs_legumes),
                                           rank == 3 ~ as.character(fsl_fcs_veg), 
                                           rank == 4 ~ as.character(fsl_fcs_fruit), 
                                           rank == 5 ~ as.character(fsl_fcs_condiments), 
                                           rank == 6 ~ as.character(fsl_fcs_meat), 
                                           rank == 7 ~ as.character(fsl_fcs_dairy), 
                                           rank == 8 ~ as.character(fsl_fcs_sugar), 
                                           rank == 9 ~ as.character(fsl_fcs_oil), 
                                           rank == 10 ~ as.character(fsl_hhs_nofoodh), 
                                           rank == 11 ~ as.character(fsl_hhs_nofoodhh_freq), 
                                           rank == 12 ~ as.character(fsl_hhs_sleephungry), 
                                           rank == 13 ~ as.character(fsl_hhs_sleephungry_freq), 
                                           rank == 14 ~ as.character(fsl_hhs_alldaynight), 
                                           rank == 15 ~ as.character(fsl_hhs_alldaynight_freq), 
                                           rank == 16 ~ as.character(fsl_rcsi_lessquality), 
                                           rank == 17 ~ as.character(fsl_rcsi_mealsize), 
                                           rank == 18 ~ as.character(fsl_rcsi_mealadult), 
                                           rank == 19 ~ as.character(fsl_rcsi_mealnb), 
                                           TRUE ~ as.character(fsl_rcsi_borrow))
  ) |> 
  filter(!is.na(i.check.current_value), !i.check.current_value %in% c("0")) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_fd_hhs_severe_but_rcsi_low")

# - low FCS and low rCSI. fcs [poor] // rcs[0] *** take out all the data **
df_logic_c_fd_fcs_poor_but_rcsi_low <- df_tool_data |>  
  filter(i.hhs_cat %in% c("Poor"), i.rcsi_cat %in% c("rcsi_0_3"))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_fcs_cereal",
         i.check.current_value = as.character(fsl_fcs_cereal),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_fd_hhs_severe_but_rcsi_low",
         i.check.issue = glue("hhs_severe_but_rcsi_low"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  slice(rep(1:n(), each = 20)) |>  
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fsl_fcs_cereal", 
                                  rank == 2 ~ "fsl_fcs_legumes",
                                  rank == 3 ~ "fsl_fcs_veg", 
                                  rank == 4 ~ "fsl_fcs_fruit", 
                                  rank == 5 ~ "fsl_fcs_condiments", 
                                  rank == 6 ~ "fsl_fcs_meat", 
                                  rank == 7 ~ "fsl_fcs_dairy", 
                                  rank == 8 ~ "fsl_fcs_sugar", 
                                  rank == 9 ~ "fsl_fcs_oil", 
                                  rank == 10 ~ "fsl_hhs_nofoodh", 
                                  rank == 11 ~ "fsl_hhs_nofoodhh_freq", 
                                  rank == 12 ~ "fsl_hhs_sleephungry", 
                                  rank == 13 ~ "fsl_hhs_sleephungry_freq", 
                                  rank == 14 ~ "fsl_hhs_alldaynight", 
                                  rank == 15 ~ "fsl_hhs_alldaynight_freq", 
                                  rank == 16 ~ "fsl_rcsi_lessquality", 
                                  rank == 17 ~ "fsl_rcsi_mealsize", 
                                  rank == 18 ~ "fsl_rcsi_mealadult", 
                                  rank == 19 ~ "fsl_rcsi_mealnb", 
                                  TRUE ~ "fsl_rcsi_borrow"),
         i.check.value = case_when(i.check.name %in% c("fsl_hhs_nofoodh", 
                                                       "fsl_hhs_sleephungry",
                                                       "fsl_hhs_alldaynight") ~ "0", 
                                   TRUE ~ i.check.value),
         i.check.current_value = case_when(rank == 1 ~ as.character(fsl_fcs_cereal),
                                           rank == 2 ~ as.character(fsl_fcs_legumes),
                                           rank == 3 ~ as.character(fsl_fcs_veg), 
                                           rank == 4 ~ as.character(fsl_fcs_fruit), 
                                           rank == 5 ~ as.character(fsl_fcs_condiments), 
                                           rank == 6 ~ as.character(fsl_fcs_meat), 
                                           rank == 7 ~ as.character(fsl_fcs_dairy), 
                                           rank == 8 ~ as.character(fsl_fcs_sugar), 
                                           rank == 9 ~ as.character(fsl_fcs_oil), 
                                           rank == 10 ~ as.character(fsl_hhs_nofoodh), 
                                           rank == 11 ~ as.character(fsl_hhs_nofoodhh_freq), 
                                           rank == 12 ~ as.character(fsl_hhs_sleephungry), 
                                           rank == 13 ~ as.character(fsl_hhs_sleephungry_freq), 
                                           rank == 14 ~ as.character(fsl_hhs_alldaynight), 
                                           rank == 15 ~ as.character(fsl_hhs_alldaynight_freq), 
                                           rank == 16 ~ as.character(fsl_rcsi_lessquality), 
                                           rank == 17 ~ as.character(fsl_rcsi_mealsize), 
                                           rank == 18 ~ as.character(fsl_rcsi_mealadult), 
                                           rank == 19 ~ as.character(fsl_rcsi_mealnb), 
                                           TRUE ~ as.character(fsl_rcsi_borrow))
  ) |> 
  filter(!is.na(i.check.current_value), !i.check.current_value %in% c("0")) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_fd_fcs_poor_but_rcsi_low")

# - high FCS and high rCSI
df_logic_c_fd_fcs_acceptable_but_rcsi_high <- df_tool_data |>  
  filter(i.fcs_cat %in% c("Acceptable"), i.rcsi_cat %in% c("rcsi_19+"))  |>
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_fcs_cereal",
         i.check.current_value = as.character(fsl_fcs_cereal),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_fd_hhs_severe_but_rcsi_low",
         i.check.issue = glue("hhs_severe_but_rcsi_low"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  slice(rep(1:n(), each = 20)) |>  
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fsl_fcs_cereal", 
                                  rank == 2 ~ "fsl_fcs_legumes",
                                  rank == 3 ~ "fsl_fcs_veg", 
                                  rank == 4 ~ "fsl_fcs_fruit", 
                                  rank == 5 ~ "fsl_fcs_condiments", 
                                  rank == 6 ~ "fsl_fcs_meat", 
                                  rank == 7 ~ "fsl_fcs_dairy", 
                                  rank == 8 ~ "fsl_fcs_sugar", 
                                  rank == 9~ "fsl_fcs_oil", 
                                  rank == 10 ~ "fsl_hhs_nofoodh", 
                                  rank == 11 ~ "fsl_hhs_nofoodhh_freq", 
                                  rank == 12 ~ "fsl_hhs_sleephungry", 
                                  rank == 13 ~ "fsl_hhs_sleephungry_freq", 
                                  rank == 14 ~ "fsl_hhs_alldaynight", 
                                  rank == 15 ~ "fsl_hhs_alldaynight_freq", 
                                  rank == 16 ~ "fsl_rcsi_lessquality", 
                                  rank == 17 ~ "fsl_rcsi_mealsize", 
                                  rank == 18 ~ "fsl_rcsi_mealadult", 
                                  rank == 19 ~ "fsl_rcsi_mealnb", 
                                  TRUE ~ "fsl_rcsi_borrow"),
         i.check.value = case_when(i.check.name %in% c("fsl_hhs_nofoodh", 
                                                       "fsl_hhs_sleephungry",
                                                       "fsl_hhs_alldaynight") ~ "0", 
                                   TRUE ~ i.check.value),
         i.check.current_value = case_when(rank == 1 ~ as.character(fsl_fcs_cereal),
                                           rank == 2 ~ as.character(fsl_fcs_legumes),
                                           rank == 3 ~ as.character(fsl_fcs_veg), 
                                           rank == 4 ~ as.character(fsl_fcs_fruit), 
                                           rank == 5 ~ as.character(fsl_fcs_condiments), 
                                           rank == 6 ~ as.character(fsl_fcs_meat), 
                                           rank == 7 ~ as.character(fsl_fcs_dairy), 
                                           rank == 8 ~ as.character(fsl_fcs_sugar), 
                                           rank == 9 ~ as.character(fsl_fcs_oil), 
                                           rank == 10 ~ as.character(fsl_hhs_nofoodh), 
                                           rank == 11 ~ as.character(fsl_hhs_nofoodhh_freq), 
                                           rank == 12 ~ as.character(fsl_hhs_sleephungry), 
                                           rank == 13 ~ as.character(fsl_hhs_sleephungry_freq), 
                                           rank == 14 ~ as.character(fsl_hhs_alldaynight), 
                                           rank == 15 ~ as.character(fsl_hhs_alldaynight_freq), 
                                           rank == 16 ~ as.character(fsl_rcsi_lessquality), 
                                           rank == 17 ~ as.character(fsl_rcsi_mealsize), 
                                           rank == 18 ~ as.character(fsl_rcsi_mealadult), 
                                           rank == 19 ~ as.character(fsl_rcsi_mealnb), 
                                           TRUE ~ as.character(fsl_rcsi_borrow))
  ) |> 
  filter(!is.na(i.check.current_value), !i.check.current_value %in% c("0")) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_fd_fcs_acceptable_but_rcsi_high")

# LCSI ------------------------------------------------------------------------
# lcsi_stress_c1
df_logic_c_lcsi_stress_c1 <- df_tool_data |>  
  filter(fsl_lcsi_en_stress1 %in% c("no_exhausted"), 
         fsl_lcsi_en_stress2 %in% c("no_had_no_need"),
         fsl_lcsi_en_stress3 %in% c("no_had_no_need"))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_lcsi_en_stress1",
         i.check.current_value = as.character(fsl_lcsi_en_stress1),
         i.check.value = "",
         i.check.issue_id = "logic_c_lcsi_stress_c1",
         i.check.issue = glue("lcsi_stress_c1"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  slice(rep(1:n(), each = 10)) |>  
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fsl_lcsi_en_stress1", 
                                  rank == 2 ~ "fsl_lcsi_en_stress2",
                                  rank == 3 ~ "fsl_lcsi_en_stress3", 
                                  rank == 4 ~ "fsl_lcsi_en_stress4", 
                                  rank == 5 ~ "fsl_lcsi_en_crisis1", 
                                  rank == 6 ~ "fsl_lcsi_en_crisis2", 
                                  rank == 7 ~ "fsl_lcsi_en_crisis3", 
                                  rank == 8 ~ "fsl_lcsi_en_emergency1", 
                                  rank == 9 ~ "fsl_lcsi_en_emergency2", 
                                  TRUE ~ "fsl_lcsi_en_emergency3"),
         i.check.current_value = case_when(rank == 1 ~ as.character(fsl_lcsi_en_stress1),
                                           rank == 2 ~ as.character(fsl_lcsi_en_stress2),
                                           rank == 3 ~ as.character(fsl_lcsi_en_stress3), 
                                           rank == 4 ~ as.character(fsl_lcsi_en_stress4), 
                                           rank == 5 ~ as.character(fsl_lcsi_en_crisis1), 
                                           rank == 6 ~ as.character(fsl_lcsi_en_crisis2), 
                                           rank == 7 ~ as.character(fsl_lcsi_en_crisis3), 
                                           rank == 8 ~ as.character(fsl_lcsi_en_emergency1), 
                                           rank == 9 ~ as.character(fsl_lcsi_en_emergency2), 
                                           TRUE ~ as.character(fsl_lcsi_en_emergency3))
  ) |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_lcsi_stress_c1")

# lcsi_stress_c2
df_logic_c_lcsi_stress_c2 <- df_tool_data |>  
  filter(fsl_lcsi_en_stress1 %in% c("no_exhausted"), 
         fsl_lcsi_en_stress2 %in% c("no_had_no_need"),
         fsl_lcsi_en_stress3 %in% c("no_had_no_need"))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_lcsi_en_stress1",
         i.check.current_value = as.character(fsl_lcsi_en_stress1),
         i.check.value = "NA",
         i.check.issue_id = "logic_c_lcsi_stress_c2",
         i.check.issue = glue("lcsi_stress_c2"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  slice(rep(1:n(), each = 10)) |>  
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fsl_lcsi_en_stress1", 
                                  rank == 2 ~ "fsl_lcsi_en_stress2",
                                  rank == 3 ~ "fsl_lcsi_en_stress3", 
                                  rank == 4 ~ "fsl_lcsi_en_stress4", 
                                  rank == 5 ~ "fsl_lcsi_en_crisis1", 
                                  rank == 6 ~ "fsl_lcsi_en_crisis2", 
                                  rank == 7 ~ "fsl_lcsi_en_crisis3", 
                                  rank == 8 ~ "fsl_lcsi_en_emergency1", 
                                  rank == 9 ~ "fsl_lcsi_en_emergency2", 
                                  TRUE ~ "fsl_lcsi_en_emergency3"),
         i.check.current_value = case_when(rank == 1 ~ as.character(fsl_lcsi_en_stress1),
                                           rank == 2 ~ as.character(fsl_lcsi_en_stress2),
                                           rank == 3 ~ as.character(fsl_lcsi_en_stress3), 
                                           rank == 4 ~ as.character(fsl_lcsi_en_stress4), 
                                           rank == 5 ~ as.character(fsl_lcsi_en_crisis1), 
                                           rank == 6 ~ as.character(fsl_lcsi_en_crisis2), 
                                           rank == 7 ~ as.character(fsl_lcsi_en_crisis3), 
                                           rank == 8 ~ as.character(fsl_lcsi_en_emergency1), 
                                           rank == 9 ~ as.character(fsl_lcsi_en_emergency2), 
                                           TRUE ~ as.character(fsl_lcsi_en_emergency3))
  ) |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_lcsi_stress_c2")

# lcsi_stress_c3
df_logic_c_lcsi_stress_c3 <- df_tool_data |>  
  filter(fsl_lcsi_en_stress1 %in% c("no_had_no_need"), 
         fsl_lcsi_en_stress2 %in% c("no_exhausted"),
         fsl_lcsi_en_stress3 %in% c("no_had_no_need"),
         fsl_lcsi_en_emergency1 %in% c("no_exhausted"))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_lcsi_en_stress1",
         i.check.current_value = as.character(fsl_lcsi_en_stress1),
         i.check.value = "",
         i.check.issue_id = "logic_c_lcsi_stress_c3",
         i.check.issue = glue("lcsi_stress_c3"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  slice(rep(1:n(), each = 10)) |>  
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fsl_lcsi_en_stress1", 
                                  rank == 2 ~ "fsl_lcsi_en_stress2",
                                  rank == 3 ~ "fsl_lcsi_en_stress3", 
                                  rank == 4 ~ "fsl_lcsi_en_stress4", 
                                  rank == 5 ~ "fsl_lcsi_en_crisis1", 
                                  rank == 6 ~ "fsl_lcsi_en_crisis2", 
                                  rank == 7 ~ "fsl_lcsi_en_crisis3", 
                                  rank == 8 ~ "fsl_lcsi_en_emergency1", 
                                  rank == 9 ~ "fsl_lcsi_en_emergency2", 
                                  TRUE ~ "fsl_lcsi_en_emergency3"),
         i.check.current_value = case_when(rank == 1 ~ as.character(fsl_lcsi_en_stress1),
                                           rank == 2 ~ as.character(fsl_lcsi_en_stress2),
                                           rank == 3 ~ as.character(fsl_lcsi_en_stress3), 
                                           rank == 4 ~ as.character(fsl_lcsi_en_stress4), 
                                           rank == 5 ~ as.character(fsl_lcsi_en_crisis1), 
                                           rank == 6 ~ as.character(fsl_lcsi_en_crisis2), 
                                           rank == 7 ~ as.character(fsl_lcsi_en_crisis3), 
                                           rank == 8 ~ as.character(fsl_lcsi_en_emergency1), 
                                           rank == 9 ~ as.character(fsl_lcsi_en_emergency2), 
                                           TRUE ~ as.character(fsl_lcsi_en_emergency3))
  ) |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_lcsi_stress_c3")

# lcsi_stress_c4
df_logic_c_lcsi_stress4_but_no_emergency_c4 <- df_tool_data |>  
  filter(fsl_lcsi_en_stress4 %in% c("no_exhausted"), 
         fsl_lcsi_en_emergency2 %in% c("not_applicable"))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_lcsi_en_stress1",
         i.check.current_value = as.character(fsl_lcsi_en_stress1),
         i.check.value = "",
         i.check.issue_id = "logic_c_lcsi_stress4_but_no_emergency_c4",
         i.check.issue = glue("lcsi_stress4_but_no_emergency"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  slice(rep(1:n(), each = 10)) |>  
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fsl_lcsi_en_stress1", 
                                  rank == 2 ~ "fsl_lcsi_en_stress2",
                                  rank == 3 ~ "fsl_lcsi_en_stress3", 
                                  rank == 4 ~ "fsl_lcsi_en_stress4", 
                                  rank == 5 ~ "fsl_lcsi_en_crisis1", 
                                  rank == 6 ~ "fsl_lcsi_en_crisis2", 
                                  rank == 7 ~ "fsl_lcsi_en_crisis3", 
                                  rank == 8 ~ "fsl_lcsi_en_emergency1", 
                                  rank == 9 ~ "fsl_lcsi_en_emergency2", 
                                  TRUE ~ "fsl_lcsi_en_emergency3"),
         i.check.current_value = case_when(rank == 1 ~ as.character(fsl_lcsi_en_stress1),
                                           rank == 2 ~ as.character(fsl_lcsi_en_stress2),
                                           rank == 3 ~ as.character(fsl_lcsi_en_stress3), 
                                           rank == 4 ~ as.character(fsl_lcsi_en_stress4), 
                                           rank == 5 ~ as.character(fsl_lcsi_en_crisis1), 
                                           rank == 6 ~ as.character(fsl_lcsi_en_crisis2), 
                                           rank == 7 ~ as.character(fsl_lcsi_en_crisis3), 
                                           rank == 8 ~ as.character(fsl_lcsi_en_emergency1), 
                                           rank == 9 ~ as.character(fsl_lcsi_en_emergency2), 
                                           TRUE ~ as.character(fsl_lcsi_en_emergency3))
  ) |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_lcsi_stress4_but_no_emergency_c4")

# lcsi_stress_c5
df_logic_c_lcsi_no_stress4_but_emergency2_c5 <- df_tool_data |>  
  filter(fsl_lcsi_en_stress4 %in% c("no_had_no_need", "not_applicable"), 
         fsl_lcsi_en_emergency2 %in% c("yes", "no_exhausted"))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_lcsi_en_emergency2",
         i.check.current_value = as.character(fsl_lcsi_en_emergency2),
         i.check.value = "not_applicable",
         i.check.issue_id = "logic_c_lcsi_no_stress4_but_emergency2_c5",
         i.check.issue = glue("no stress4 but emergency2"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "")  

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_lcsi_no_stress4_but_emergency2_c5")
  
# lcsi_stress_c6
df_logic_c_lcsi_no_stress_but_crisis_emergency_c6 <- df_tool_data |>  
  filter(fsl_lcsi_en_stress1 %in% c("no_had_no_need", "not_applicable"), 
         fsl_lcsi_en_stress2 %in% c("no_had_no_need", "not_applicable"),
         fsl_lcsi_en_stress3 %in% c("no_had_no_need", "not_applicable"),
         fsl_lcsi_en_stress4 %in% c("no_had_no_need", "not_applicable"),
         (fsl_lcsi_en_crisis1 %in% c("yes", "no_exhausted")|fsl_lcsi_en_crisis2 %in% c("yes", "no_exhausted")|fsl_lcsi_en_crisis3 %in% c("yes", "no_exhausted")|
            fsl_lcsi_en_emergency1 %in% c("yes", "no_exhausted")|fsl_lcsi_en_emergency2 %in% c("yes", "no_exhausted")|fsl_lcsi_en_emergency3 %in% c("yes", "no_exhausted")))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_lcsi_en_stress1",
         i.check.current_value = as.character(fsl_lcsi_en_stress1),
         i.check.value = "not_applicable",
         i.check.issue_id = "logic_c_lcsi_no_stress_but_crisis_emergency_c6",
         i.check.issue = glue("No stress reported but crisis and emergency"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  slice(rep(1:n(), each = 6)) |>  
  group_by(i.check.uuid, i.check.start_date, i.check.enumerator_id, i.check.type,  i.check.name,  i.check.current_value) |>  
  mutate(rank = row_number(),
         i.check.name = case_when(rank == 1 ~ "fsl_lcsi_en_crisis1", 
                                  rank == 2 ~ "fsl_lcsi_en_crisis2", 
                                  rank == 3 ~ "fsl_lcsi_en_crisis3", 
                                  rank == 4 ~ "fsl_lcsi_en_emergency1", 
                                  rank == 5 ~ "fsl_lcsi_en_emergency2", 
                                  TRUE ~ "livh_emerg_lcsi_3"),
         i.check.current_value = case_when(rank == 1 ~ as.character(fsl_lcsi_en_crisis1), 
                                           rank == 2 ~ as.character(fsl_lcsi_en_crisis2), 
                                           rank == 3 ~ as.character(fsl_lcsi_en_crisis3), 
                                           rank == 4 ~ as.character(fsl_lcsi_en_emergency1), 
                                           rank == 5 ~ as.character(fsl_lcsi_en_emergency2), 
                                           TRUE ~ as.character(fsl_lcsi_en_emergency3))
  ) |> 
  filter(!i.check.current_value %in% c("no_had_no_need", "not_applicable")) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_lcsi_no_stress_but_crisis_emergency_c6")

# lcsi_stress_c7
df_logic_c_lcsi_no_livestock_but_stress4_c7 <- df_tool_data |>  
  filter(cm_assets_ownership_agriculture %in% c("none"), 
         fsl_lcsi_en_stress4 %in% c("yes"))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_lcsi_en_stress4",
         i.check.current_value = as.character(fsl_lcsi_en_stress4),
         i.check.value = "no_exhausted",
         i.check.issue_id = "logic_c_lcsi_no_livestock_but_stress4_c7",
         i.check.issue = glue("livestock but stress4"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_lcsi_no_livestock_but_stress4_c7")

# lcsi_stress_c8
df_logic_c_lcsi_no_livestock_but_emergency2_c8 <- df_tool_data |>  
  filter(cm_assets_ownership_agriculture %in% c("none"), 
         fsl_lcsi_en_emergency2 %in% c("yes"))  |> 
  mutate(i.check.type = "change_response",
         i.check.name = "fsl_lcsi_en_emergency2",
         i.check.current_value = as.character(fsl_lcsi_en_emergency2),
         i.check.value = "no_exhausted",
         i.check.issue_id = "logic_c_lcsi_no_livestock_but_emergency2_c8",
         i.check.issue = glue("livestock but emergency2"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.so_sm_choices = "") |> 
  filter(!is.na(i.check.current_value)) |> 
  supporteR::batch_select_rename(input_selection_str = "i.check.", input_replacement_str = "") 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_logic_c_lcsi_no_livestock_but_emergency2_c8")

# log 999
cols_with_integer_values <- df_survey |> filter(type %in% c("integer")) |> pull(name)

df_999_data <- purrr::map_dfr(.x = cols_with_integer_values, 
                              .f = ~ {df_tool_data |> 
                                  dplyr::filter(str_detect(string = !!sym(.x), pattern = "^-[9]{2,4}$|^[9]{2,4}$")) |> 
                                  dplyr::mutate(i.check.type = "change_response",
                                                i.check.name = .x,
                                                i.check.current_value = as.character(!!sym(.x)),
                                                i.check.value = "",
                                                i.check.issue_id = "logic_c_handle_999",
                                                i.check.issue = "remove 999 added during data collection",
                                                i.check.other_text = "",
                                                i.check.checked_by = "",
                                                i.check.checked_date = as_date(today()),
                                                i.check.comment = "",
                                                i.check.reviewed = "",
                                                i.check.adjust_log = "",
                                                i.check.so_sm_choices = "") |>
                                  dplyr::select(starts_with("i.check."))}) |> 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_999_data")

# log 999
cols_with_text_values <- df_survey |> filter(type %in% c("text"), name %in% colnames(df_tool_data)) |> pull(name)

df_999_data_other <- purrr::map_dfr(.x = cols_with_text_values, 
                                    .f = ~ {df_tool_data |> 
                                        dplyr::filter(str_detect(string = !!sym(.x), pattern = "^-[9]{2,4}$|^[9]{2,4}$")) |> 
                                        dplyr::mutate(i.check.type = "change_response",
                                                      i.check.name = .x,
                                                      i.check.current_value = as.character(!!sym(.x)),
                                                      i.check.value = "",
                                                      i.check.issue_id = "logic_c_handle_999_other",
                                                      i.check.issue = "remove 999 added during data collection",
                                                      i.check.other_text = "",
                                                      i.check.checked_by = "",
                                                      i.check.checked_date = as_date(today()),
                                                      i.check.comment = "",
                                                      i.check.reviewed = "",
                                                      i.check.adjust_log = "",
                                                      i.check.so_sm_choices = "") |>
                                        dplyr::select(starts_with("i.check."))}) |> 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = "")) 

add_checks_data_to_list(input_list_name = "checks_output", input_df_name = "df_999_data_other")

# combined  checks ------------------------------------------------------------

df_combined_checks <- bind_rows(checks_output)

# output the log --------------------------------------------------------------

write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), 
                                                "_combined_checks_eth_msna.csv"), na = "")

###############################################################################

