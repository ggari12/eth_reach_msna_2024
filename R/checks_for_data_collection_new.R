###############################################################################
# checks for data collection

# read packages
library(tidyverse)
library(cleaningtools)
library(httr)
library(sf)
library(glue)
library(supporteR)
library(openxlsx)
library(cluster)

source("R/support_functions.R")
source("R/composite_indicators.R")
source("support_files/credentials.R")

# read data -------------------------------------------------------------------

data_path <- "inputs/ETH2403_MSNA_2024_data_all.xlsx" 

# main data
data_nms <- names(readxl::read_excel(path = data_path, n_max = 300))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")
df_tool_data <- readxl::read_excel(data_path, col_types = c_types) |> 
  mutate(`_geopoint_latitude` = as.numeric(`_household_geopoint_latitude`),
         `_geopoint_longitude` = as.numeric(`_household_geopoint_longitude`)) |> 
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
# roster
data_nms_roster <- names(readxl::read_excel(path = data_path, n_max = 300, sheet = "roster"))
c_types_roster <- ifelse(str_detect(string = data_nms_roster, pattern = "_other$"), "text", "guess")
df_loop_roster <- readxl::read_excel(data_path, col_types = c_types_roster, sheet = "roster")

# education
data_nms_educ <- names(readxl::read_excel(path = data_path, n_max = 300, sheet = "edu_ind"))
c_types_educ <- ifelse(str_detect(string = data_nms_educ, pattern = "_other$"), "text", "guess")
df_loop_educ <- readxl::read_excel(data_path, col_types = c_types_educ, sheet = "edu_ind")

# health
data_nms_health <- names(readxl::read_excel(path = data_path, n_max = 300, sheet = "health_ind"))
c_types_health <- ifelse(str_detect(string = data_nms_health, pattern = "_other$"), "text", "guess")
df_loop_health <- readxl::read_excel(data_path, col_types = c_types_health, sheet = "health_ind")

# civil
data_nms_civil <- names(readxl::read_excel(path = data_path, n_max = 300, sheet = "civil_ind"))
c_types_civil <- ifelse(str_detect(string = data_nms_civil, pattern = "_other$"), "text", "guess")
df_loop_civil <- readxl::read_excel(data_path, col_types = c_types_civil, sheet = "civil_ind")

# hazard_concern
data_nms_hazard_conc <- names(readxl::read_excel(path = data_path, n_max = 300, sheet = "hazard_concern_rep"))
c_types_hazard_conc <- ifelse(str_detect(string = data_nms_hazard_conc, pattern = "_other$"), "text", "guess")
df_loop_hazard_conc <- readxl::read_excel(data_path, col_types = c_types_hazard_conc, sheet = "hazard_concern_rep")

# hazard_type
data_nms_hazard_type <- names(readxl::read_excel(path = data_path, n_max = 300, sheet = "hazard_type_rep"))
c_types_hazard_type <- ifelse(str_detect(string = data_nms_hazard_type, pattern = "_other$"), "text", "guess")
df_loop_hazard_type <- readxl::read_excel(data_path, col_types = c_types_hazard_type, sheet = "hazard_type_rep")

# nutrition
data_nms_nut <- names(readxl::read_excel(path = data_path, n_max = 300, sheet = "nut_ind"))
c_types_nut <- ifelse(str_detect(string = data_nms_nut, pattern = "_other$"), "text", "guess")
df_loop_nut <- readxl::read_excel(data_path, col_types = c_types_nut, sheet = "nut_ind")

# joining roster loop to main shet
df_repeat_roster_data <- df_tool_data %>% 
  left_join(df_loop_roster, by = c("_uuid" = "_submission__uuid"))

df_repeat_educ_data <- df_tool_data %>% 
  left_join(df_loop_educ, by = c("_uuid" = "_submission__uuid"))

df_repeat_educ_hazard_data <- df_loop_educ %>% 
  left_join(df_loop_hazard_type, by = c("_submission__uuid" = "_submission__uuid"))

# tool
loc_tool <- "inputs/ETH2403_MSNA_2024_tool.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey") 
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

# download audit files folder
download_audit_files(df= df_tool_data,
                     uuid_column = "_uuid",
                     audit_dir = "inputs/audit_files",
                     usr = user_acc,
                     pass = user_pss)

# zip audit files folder

if (dir.exists("inputs/audit_files")) {
  zip::zip(zipfile = "inputs/audit_files.zip",
           files = list.dirs(path = "inputs/audit_files/", full.names = TRUE, recursive = FALSE),
           mode = "cherry-pick")
  
}

# GIS layer for samples
df_sample_data <- sf::st_read("inputs/final_msna_samples.gpkg", quiet = TRUE) %>% 
  filter(cluster_typ %in% c("init", "repl"))

# check pii ---------------------------------------------------------------
pii_cols <- c("telephone", "contact", "name", "gps", "latitude", "longitude", "contact", "geopoint")
pii_from_data <- cleaningtools::check_pii(dataset = df_tool_data, element_name = "checked_dataset", uuid_column = "_uuid")
pii_from_data$potential_PII

# duration ----------------------------------------------------------------
# read audit file
audit_list_data <- cleaningtools::create_audit_list(audit_zip_path = "inputs/audit_files.zip")

# add duration from audit
df_tool_data_with_audit_time <- cleaningtools::add_duration_from_audit(df_tool_data, uuid_column = "_uuid", audit_list = audit_list_data)

# outliers columns not to check
outlier_cols_not_4_checking <- df_tool_data %>% 
  select(matches("numsides|enum_age|sum|Repeat_hct|Repeat_ht|i.fcs|i.rcsi|i.hhs|geopoint|gps|_index|_submit|submission|_sample_|^_id$")) %>% 
  colnames()

# logical checks data ----------------------------------------------------
df_list_logical_checks <- read_csv("support_files/logical_check_list.csv") %>% 
  filter(!is.na(check_id))

# combine cleaning tools checks
list_log <- df_tool_data_with_audit_time %>%
  check_duration(column_to_check = "duration_audit_sum_all_minutes",
                 uuid_column = "_uuid",
                 log_name = "duration_log",
                 lower_bound = 30,
                 higher_bound = 120) %>% 
  check_outliers(uuid_column = "_uuid", sm_separator = "/",
                 strongness_factor = 3, columns_not_to_check = outlier_cols_not_4_checking) %>% 
  check_soft_duplicates(kobo_survey = df_survey,
                        uuid_column = "_uuid",
                        idnk_value = "dnk",
                        sm_separator = "/",
                        log_name = "soft_duplicate_log",
                        threshold = 25,
                        return_all_results = FALSE) %>%
  check_value(uuid_column = "_uuid", values_to_look = c(99, 999, 9999)) %>% 
  check_logical_with_list(uuid_column = "_uuid",
                          list_of_check = df_list_logical_checks,
                          check_id_column = "check_id",
                          check_to_perform_column = "check_to_perform",
                          columns_to_clean_column = "columns_to_clean",
                          description_column = "description",
                          bind_checks = TRUE)

# others checks ---------------------------------------------------------------
df_other_checks <- cts_other_specify(input_tool_data = df_tool_data %>% select(-fsl_lcsi_na_other,
                                                                               -fsl_lcsi_other,
                                                                               -fsl_lcsi_disp_other), 
                                     input_uuid_col = "_uuid", 
                                     input_survey = df_survey, 
                                     input_choices = df_choices)
# add other checks to the list
list_log$other_log <- df_other_checks

# repeat_other_specify --------------------------------------------------------
df_repeat_other_checks <- cts_other_specify_repeats(input_repeat_data = df_loop_health,
                                                    input_uuid_col = "_submission__uuid",
                                                    input_survey = df_survey,
                                                    input_choices = df_choices,
                                                    input_sheet_name = "health_ind",
                                                    input_index_col = "_index")

# add repeat other checks to the list
list_log$repeat_other_log <- df_repeat_other_checks

df_repeat_other_checks2 <- cts_other_specify_repeats(input_repeat_data = df_loop_educ,
                                                     input_uuid_col = "_submission__uuid",
                                                     input_survey = df_survey,
                                                     input_choices = df_choices,
                                                     input_sheet_name = "edu_ind",
                                                     input_index_col = "_index")

# add repeat other checks to the list
list_log$repeat_other_log2 <- df_repeat_other_checks2

df_repeat_other_checks3 <- cts_other_specify_repeats(input_repeat_data = df_loop_hazard_conc,
                                                     input_uuid_col = "_submission__uuid",
                                                     input_survey = df_survey,
                                                     input_choices = df_choices,
                                                     input_sheet_name = "hazard_concern_rep",
                                                     input_index_col = "_index")

# add repeat other checks to the list
list_log$repeat_other_log3 <- df_repeat_other_checks3

df_repeat_other_checks4 <- cts_other_specify_repeats(input_repeat_data = df_loop_hazard_type,
                                                     input_uuid_col = "_submission__uuid",
                                                     input_survey = df_survey,
                                                     input_choices = df_choices,
                                                     input_sheet_name = "hazard_type_rep",
                                                     input_index_col = "_index")

# add repeat other checks to the list
list_log$repeat_other_log4 <- df_repeat_other_checks4

df_repeat_other_checks5 <- cts_other_specify_repeats(input_repeat_data = df_loop_nut,
                                                     input_uuid_col = "_submission__uuid",
                                                     input_survey = df_survey,
                                                     input_choices = df_choices,
                                                     input_sheet_name = "nut_ind",
                                                     input_index_col = "_index")

# add repeat other checks to the list
list_log$repeat_other_log5 <- df_repeat_other_checks5

# other_expenditure_other_checks
df_other_expenditure_check <- df_tool_data %>% 
  filter(!is.na(cm_expenditure_frequent_others_other)) %>% 
  mutate(i.check.uuid = `_uuid`,
         i.check.change_type = "change_response",
         i.check.question = "cm_expenditure_frequent_others_other",
         i.check.old_value = as.character(cm_expenditure_frequent_others_other),
         i.check.new_value = "NA",
         i.check.issue = "recode other",
         i.check.description = "",
         i.check.other_text = "",
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.so_sm_choices = "") %>% 
  batch_select_rename()

list_log$other_expenditure_other_log <- df_other_expenditure_check

# other_expenditure_other_checks
df_other_expenditure_infrequent_check <- df_tool_data %>% 
  filter(!is.na(cm_expenditure_infrequent_other)) %>% 
  mutate(i.check.uuid = `_uuid`,
         i.check.change_type = "change_response",
         i.check.question = "cm_expenditure_infrequent_other",
         i.check.old_value = as.character(cm_expenditure_infrequent_other),
         i.check.new_value = "NA",
         i.check.issue = "recode other",
         i.check.description = "",
         i.check.other_text = "",
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.so_sm_choices = "") %>% 
  batch_select_rename()

list_log$other_expenditure_infrequent_other_log <- df_other_expenditure_infrequent_check

# testing data ----------------------------------------------------------------
df_testing_data_check <- df_tool_data %>%
  filter(start < as_date("2024-06-26")) %>% 
  mutate(i.check.uuid = `_uuid`,
         i.check.change_type = "remove_survey",
         i.check.question = "",
         i.check.old_value = "",
         i.check.new_value = "",
         i.check.issue = "testing_data",
         i.check.description = "logic_c_testing_data",
         i.check.other_text = "",
         i.check.comment = "",
         i.check.reviewed = "1",
         i.check.so_sm_choices = "") %>% 
  batch_select_rename() 

list_log$df_testing_data_log <- df_testing_data_check

# no consent -----------------------------------------------------------------
df_no_consent_check <- df_tool_data %>% 
  filter(consent == "no") %>%  
  mutate(i.check.uuid = `_uuid`,
         i.check.change_type = "remove_survey",
         i.check.question = "",
         i.check.old_value = "",
         i.check.new_value = "",
         i.check.issue = "no_consent",
         i.check.description = "logic_c_requirement_no_consent",
         i.check.other_text = "",
         i.check.comment = "",
         i.check.reviewed = "1",
         i.check.so_sm_choices = "") %>% 
  batch_select_rename() 

# add other checks to the list
list_log$df_no_consent_log <- df_no_consent_check

# check duplicate uuids ---------------------------------------------------
df_duplicate_uuids <- cts_checks_duplicate_uuids(input_tool_data = df_tool_data)
list_log$duplicate_uuid_log <- df_duplicate_uuids

# loops outliers ----------------------------------------------------------
# roster
#df_loop_outliers_roster <- cleaningtools::check_outliers(dataset = df_loop_roster %>% select(-ind_pos,
#                                                                                             -check_age,
#                                                                                             -check_gender,
#                                                                                             -`_parent_index`) %>%  
#                                                           mutate(loop_uuid = paste0(`_submission__uuid`, " * ", `_index`)), 
#                                                           uuid_column = "loop_uuid", strongness_factor = 3,
#                                                           sm_separator = "/") 
#
#df_potential_loop_outliers_roster <- df_loop_outliers_roster$potential_outliers %>% 
#  separate_wider_delim(cols = uuid, delim = " * ", names = c("i.check.uuid", "index")) %>% 
#  mutate(i.check.change_type = "change_response",
#         i.check.question = question,
#         i.check.old_value = as.character(old_value),
#         i.check.new_value = "NA",
#         i.check.issue = issue,
#         i.check.description = "",
#         i.check.other_text = "",
#         i.check.comment = "",
#         i.check.reviewed = "",
#         i.check.so_sm_choices = "",
#         i.check.sheet = "roster",
#         i.check.index = index) %>% 
#  batch_select_rename()
#
#list_log$outliers_roster_log <- df_potential_loop_outliers_roster

# spatial checks ----------------------------------------------------------
if("status" %in% colnames(df_sample_data)){
  sample_pt_nos <- df_sample_data %>%
    mutate(unique_pt_number = paste0(status, "_", Name)) %>%
    pull(unique_pt_number) %>%
    unique()
}else{
  sample_pt_nos <- df_sample_data %>%
    mutate(unique_pt_number = Name) %>%
    pull(unique_pt_number) %>%
    unique()
}

# duplicate point numbers
df_duplicate_pt_nos <- cts_check_duplicate_pt_numbers(input_tool_data = df_tool_data,
                                                      input_uuid_col  = "_uuid",
                                                      input_location_col = "admin4",
                                                      input_point_id_col = "point_number",
                                                      input_sample_pt_nos_list = sample_pt_nos)

list_log$duplicate_pt_nos <- df_duplicate_pt_nos

# point number does not exist in sample
df_pt_number_not_in_sample <- cts_check_pt_number_not_in_samples(input_tool_data = df_tool_data,
                                                                 input_uuid_col  = "_uuid",
                                                                 input_point_id_col = "point_number",
                                                                 input_sample_pt_nos_list = sample_pt_nos)
list_log$pt_number_not_in_sample <- df_pt_number_not_in_sample

# check for exceeded threshold distance
df_greater_thresh_distance <- cts_check_threshold_distance(input_sample_data = df_sample_data,
                                                           input_tool_data = df_tool_data  %>% filter(!is.na(`_geopoint_longitude`)),
                                                           input_uuid_col  = "_uuid",
                                                           input_point_id_col = "point_number",
                                                           input_threshold_dist = 150,
                                                           input_geopoint_col = "geopoint")

list_log$greater_thresh_distance <- df_greater_thresh_distance

# other logical checks --------------------------------------------------------

# Barrier to water, sanitation or health facility is disability, 
# but all washington group indicators say no difficulty 
df_logic_c_barriers_but_wgq_no_difficulty <- df_repeat_roster_data %>% 
  filter(health_barriers %in% c("disablity"),
         wash_sanitation_access_issue %in% c("disablity"),
         wash_water_access_issue %in% c("disablity"),
         wgq_vision %in% c("no_difficulty"),
         wgq_hearing %in% c("no_difficulty"),
         wgq_mobility %in% c("no_difficulty"),
         wgq_cognition %in% c("no_difficulty"),
         wgq_self_care %in% c( "no_difficulty"),
         wgq_communication %in% c( "no_difficulty")) %>%
  mutate(i.check.uuid = `_uuid`,
         i.check.change_type = "change_response",
         i.check.question = "health_barriers",
         i.check.old_value = as.character(health_barriers),
         i.check.new_value = "",
         i.check.issue = "logic_c_barriers_but_wgq_no_difficulty",
         i.check.description = "",
         i.check.other_text = "",
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.so_sm_choices = "") %>% 
  batch_select_rename()

# add checks to the list
list_log$df_logic_c_barriers_but_wgq_no_difficulty_log <- df_logic_c_barriers_but_wgq_no_difficulty

# The household reports that there is no one to look after the children 
# or the elderly during the market visit even though there are no children in the household. Check.
df_logic_c_hh_reports_access_barriers_but_no_children <- df_repeat_roster_data %>% 
  filter(cm_market_barriers_access == "nlacewvm",
         ind_age_0_4_n == 0, 
         ind_age_5_17_n == 0) %>%
  mutate(i.check.uuid = `_uuid`,
         i.check.change_type = "change_response",
         i.check.question = "cm_market_barriers_access",
         i.check.old_value = as.character(cm_market_barriers_access),
         i.check.new_value = "",
         i.check.issue = "hh_reports_access_barriers_but_no_children_in_hh",
         i.check.description = "",
         i.check.other_text = "",
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.so_sm_choices = "") %>% 
  batch_select_rename()

# add checks to the list
list_log$df_logic_c_hh_reports_access_barriers_but_no_children_log <- df_logic_c_hh_reports_access_barriers_but_no_children

# LCSI stress 6 strategy (withdrawing children from school to lack of money to cover basic needs) used, but no children of school age 
df_logic_c_hh_reports_withdrawing_children_but_no_school_aging <- df_repeat_roster_data %>% 
  filter(fsl_lcsi_en_stress6 =="yes",
         ind_age_schooling_n == 0) %>%
  mutate(i.check.uuid = `_uuid`,
         i.check.change_type = "change_response",
         i.check.question = "fsl_lcsi_en_stress6",
         i.check.old_value = as.character(fsl_lcsi_en_stress6),
         i.check.new_value = "",
         i.check.issue = glue("fsl_lcsi_en_stress6 : {fsl_lcsi_en_stress6}, but no school aging: {ind_age_schooling_n}"),
         i.check.description = "",
         i.check.other_text = "",
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.so_sm_choices = "") %>% 
  batch_select_rename()

# add checks to the list
list_log$df_logic_c_hh_reports_withdrawing_children_but_no_school_aging_log <- df_logic_c_hh_reports_withdrawing_children_but_no_school_aging

# The household reports having had to restrict adult consumption so that young children can eat 
# to cope with a lack of food but there are no children in the household. Check.
df_logic_c_hh_reports_restrict_mealadult_but_no_adult <- df_repeat_roster_data %>% 
  filter(fsl_rcsi_mealadult > 0,
         ind_age_0_4_n == 0, ind_age_5_17_n == 0) %>%
  mutate(i.check.uuid = `_uuid`,
         i.check.change_type = "change_response",
         i.check.question = "fsl_rcsi_mealadult",
         i.check.old_value = as.character(fsl_rcsi_mealadult),
         i.check.new_value = "",
         i.check.issue = "hh_reports_restrict_mealadult_but_no_adult_in_hh",
         i.check.description = "",
         i.check.other_text = "",
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.so_sm_choices = "") %>% 
  batch_select_rename()

# add checks to the list
list_log$df_logic_c_hh_reports_restrict_mealadult_but_no_adult_log <- df_logic_c_hh_reports_restrict_mealadult_but_no_adult

# Obstacle to education linked to a recent displacement or return when the household says it has never been displaced
df_logic_c_educ_enroll_displacement_but_no_displaced <- df_repeat_educ_data %>% 
  filter(edu_barrier == "enroll_displacement",
         dis_forced == "no") %>%
  mutate(i.check.uuid = `_uuid`,
         i.check.change_type = "change_response",
         i.check.question = "edu_barrier",
         i.check.old_value = as.character(edu_barrier),
         i.check.new_value = "",
         i.check.issue =  glue("edu_barrier : {edu_barrier}, but not displaced: {dis_forced}"),
         i.check.description = "",
         i.check.other_text = "",
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.so_sm_choices = "",
         i.check.sheet = "edu_ind",
         i.check.index = `_index.y`) %>% 
  batch_select_rename()

# add checks to the list
list_log$df_logic_c_educ_enroll_displacement_but_no_displaced_log <- df_logic_c_educ_enroll_displacement_but_no_displaced

# No child aged 5 to 18 in the household attends a school (formal or not) 
# while the household does not identify education as one of the most important challenges in the household. Check.
#df_logic_c_no_child_attends_school_while_hh_doesnt_identify_educ <- df_repeat_educ_data %>% 
#  filter(edu_access == "no",
#         aap_priority_challenge != "education_children") %>%
#  mutate(i.check.uuid = `_uuid`,
#         i.check.change_type = "change_response",
#         i.check.question = "edu_access",
#         i.check.old_value = as.character(edu_access),
#         i.check.new_value = "",
#         i.check.issue = glue("edu_access : {edu_access}, while_hh_doesnt_identify_educ: {aap_priority_challenge}"),
#         i.check.description = "",
#         i.check.other_text = "",
#         i.check.comment = "",
#         i.check.reviewed = "",
#         i.check.so_sm_choices = "",
#         i.check.sheet = "edu_ind",
#         i.check.index = `_index.y`) %>% 
#  batch_select_rename()
#
# add checks to the list
#list_log$df_logic_c_no_child_attends_school_while_hh_doesnt_identify_educ_log <- df_logic_c_no_child_attends_school_while_hh_doesnt_identify_educ

# Education disrupted by natural hazards but selected none to hazard type
df_logic_c_educ_disrupted_hazards_but_no_hazard_type <- df_repeat_educ_data %>% 
  filter(edu_disrupted_hazards == "yes",
         hazard_type == "none") %>%
  mutate(i.check.uuid = `_uuid`,
         i.check.change_type = "change_response",
         i.check.question = "edu_disrupted_hazards",
         i.check.old_value = as.character(edu_disrupted_hazards),
         i.check.new_value = "",
         i.check.issue =  glue("edu_disrupted_hazards : {edu_disrupted_hazards}, bu no hazard type: {hazard_type}"),
         i.check.description = "",
         i.check.other_text = "",
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.so_sm_choices = "",
         i.check.sheet = "edu_ind",
         i.check.index = `_index.y`) %>% 
  batch_select_rename()

# add checks to the list
list_log$df_logic_c_educ_disrupted_hazards_but_no_hazard_type_log <- df_logic_c_educ_disrupted_hazards_but_no_hazard_type

# Education disrupted by natural hazards but did not select access_education to hazard_impact question
df_logic_c_educ_disrupted_hazards_but_no_hazard_impact <- df_repeat_educ_hazard_data %>%  
  mutate(`_uuid` = paste0(`_submission__uuid`)) %>% 
  filter(edu_disrupted_hazards == "yes",
         hazard_impact == "no_impact|access_education") %>%
  mutate(i.check.uuid = `_uuid`,
         i.check.change_type = "change_response",
         i.check.question = "edu_disrupted_hazards",
         i.check.old_value = as.character(edu_disrupted_hazards),
         i.check.new_value = "",
         i.check.issue = glue("edu_disrupted_hazards : {edu_disrupted_hazards}, bu no report hazard impact: {hazard_impact}"),
         i.check.description = "",
         i.check.other_text = "",
         i.check.comment = "",
         i.check.reviewed = "",
         i.check.so_sm_choices = "",
         i.check.sheet = "edu_ind",
         i.check.index = `_index.y`) %>% 
  batch_select_rename()

# add checks to the list
list_log$df_logic_c_educ_disrupted_hazards_but_no_hazard_impact_log <- df_logic_c_educ_disrupted_hazards_but_no_hazard_impact

# If fsl_fcs_condiments = 0 i.e. household has not eaten salt, spices, tea, or coffee in the past seven days, surveys should be checked
df_logic_c_hh_no_eating_condiments <- df_tool_data %>% 
  filter(fsl_fcs_condiments == 0) %>%
  mutate(i.check.uuid = `_uuid`,
         i.check.change_type = "change_response",
         i.check.question = "fsl_fcs_condiments",
         i.check.old_value = as.character(fsl_fcs_condiments),
         i.check.new_value = "NA",
         i.check.issue = "hh_no_eating_condiments",
         i.check.description = "",
         i.check.other_text = "",
         i.check.comment = "enumerators misinterpreted question",
         i.check.reviewed = "",
         i.check.so_sm_choices = "")%>% 
  batch_select_rename()

# add checks to the list
list_log$hh_no_eating_condiments_log <- df_logic_c_hh_no_eating_condiments

# If fsl_fcs_legumes = 0 i.e. household has not eaten any beans/legumes, pulses or nuts in the past seven days, surveys should be checked
df_logic_c_hh_no_eating_beans_nuts <- df_tool_data %>% 
  filter(fsl_fcs_legumes == 0) %>%
  mutate(i.check.uuid = `_uuid`,
         i.check.change_type = "change_response",
         i.check.question = "fsl_fcs_legumes",
         i.check.old_value = as.character(fsl_fcs_legumes),
         i.check.new_value = "NA",
         i.check.issue = "hh_no_eating_beans_nuts",
         i.check.description = "",
         i.check.other_text = "",
         i.check.comment = "It's unlikely that a household spent 7 days eating food without any beans/legumes, pulses or nuts",
         i.check.reviewed = "",
         i.check.so_sm_choices = "") %>% 
  batch_select_rename()

# add checks to the list
list_log$hh_no_eating_beans_nuts_log <- df_logic_c_hh_no_eating_beans_nuts

# HH take short time to access the nearest health facility but report barriers:"Health facility is too far away"
#df_logic_c_hh_report_short_time_but_health_facility_far_away <- df_raw_data_loop_health_ind %>% 
#  filter(health_barriers_unmet %in% c("health_fac_far"), health_facility_time < 30) %>%
#  mutate(i.check.uuid = `_uuid`,
#         i.check.change_type = "change_response",
#         i.check.question = "health_barriers_unmet",
#         i.check.old_value = as.character(health_barriers_unmet),
#         i.check.new_value = health_barriers_unmet,
#         i.check.issue = "hh_report_short_time_but_health_facility_far_away",
#         i.check.description = "",
#         i.check.other_text = "",
#         i.check.comment = "enumerators misinterpreted question",
#         i.check.reviewed = "",
#         i.check.so_sm_choices = "") %>% 
# batch_select_rename()

# add checks to the list
#list_log$hh_report_short_time_but_health_facility_far_away_log <- df_logic_c_hh_report_short_time_but_health_facility_far_away

# check FSL -------------------------------------------------------------------

# LCSI ------------------------------------------------------------------------

# log 999
cols_with_integer_values <- df_survey  %>% filter(type %in% c("integer"))  %>% pull(name)

df_999_data <- purrr::map_dfr(.x = cols_with_integer_values,
                              .f = ~ {df_repeat_roster_data  %>% 
                                  dplyr::filter(str_detect(string = !!sym(.x), pattern = "^-[9]{2,4}$|^[9]{2,4}$")) %>%
                                  dplyr::mutate(i.check.type = "change_response",
                                                i.check.question = .x,
                                                i.check.old_value = as.character(!!sym(.x)),
                                                i.check.new_value = "",
                                                i.check.issue = " logic_c_handle_999",
                                                i.check.description = "",
                                                i.check.other_text = "",
                                                i.check.comment = "",
                                                i.check.reviewed = "",
                                                i.check.so_sm_choices = "") %>% 
                                  dplyr::select(starts_with("i.check"))}) %>% 
                                  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

# add checks to the list
list_log$logic_c_handle_999_log <- df_999_data

# log 999
cols_with_text_values <- df_survey %>% filter(type %in% c("text"), name %in% colnames(df_tool_data)) %>% pull(name)

df_999_data_other <- purrr::map_dfr(.x = cols_with_text_values, 
                                    .f = ~ {df_tool_data  %>% 
                                  dplyr::filter(str_detect(string = !!sym(.x), pattern = "^-[9]{2,4}$|^[9]{2,4}$")) %>%
                                  dplyr::mutate(i.check.type = "change_response",
                                                i.check.question = .x,
                                                i.check.old_value = as.character(!!sym(.x)),
                                                i.check.new_value = "",
                                                i.check.issue = "logic_c_handle_999_other",
                                                i.check.description = "",
                                                i.check.other_text = "",
                                                i.check.comment = "",
                                                i.check.reviewed = "",
                                                i.check.so_sm_choices = "") %>% 
                                  dplyr::select(starts_with("i.check"))}) %>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

# add checks to the list
list_log$logic_c_handle_999_other_log <- df_999_data_other

# silhouette --------------------------------------------------------------
# NOTE: the column for "col_admin" is kept in the data
omit_cols_sil <- c("start", "end", "today", "duration", "duration_minutes",
                   "deviceid", "audit", "audit_URL", "instance_name", "end_survey",
                   "geopoint", "_geopoint_latitude", "_geopoint_altitude", "_geopoint_precision", 
                   "_id" ,"_submission_time","_validation_status","_notes","_status","_submitted_by","_tags",
                   "_index" )

data_similartiy_sil <- df_tool_data %>% 
  select(- any_of(omit_cols_sil), - matches("_note$|^note_"))

df_sil_data <- calculateEnumeratorSimilarity(data = data_similartiy_sil,
                                             input_df_survey = df_survey, 
                                             col_enum = "enum_id",
                                             col_admin = "admin4") %>% 
  mutate(si2= abs(si))

df_sil_processed <- df_sil_data[order(df_sil_data$`si2`, decreasing = TRUE),!colnames(df_sil_data)%in%"si2"] %>%  
  # filter(si > 0.6) %>% 
  mutate(i.check.uuid = "all",
         i.check.question = NA_character_,
         i.check.issue = paste("silhouette flag"),
         i.check.description = glue::glue("Potential similar responses for enumerator:{enum_id}, location:{admin4}. si: {si}")) %>% 
  batch_select_rename()

# add other checks to the list
list_log$enum_similarity <- df_sil_processed

# combine the checks ------------------------------------------------------

df_combined_log <- create_combined_log_keep_change_type(dataset_name = "checked_dataset", list_of_log = list_log)

# # add_info_to_cleaning_log()
#   add_with_info <- add_info_to_cleaning_log(list_of_log = df_combined_log,
#                                           dataset = "checked_dataset",
#                                           cleaning_log = "cleaning_log",
#                                           dataset_uuid_column = "_uuid",
#                                           cleaning_log_uuid_column = "uuid",
#                                           information_to_add = c("enum_id", "today", "admin4"))
 
# # create_xlsx_cleaning_log()
#   add_with_info %>%
#     create_xlsx_cleaning_log(cleaning_log_name = "cleaning_log",
#                              change_type_col = "change_type",
#                              column_for_color = "check_binding",
#                              kobo_survey = df_survey,
#                              kobo_choices = df_choices,
#                              use_dropdown = TRUE,
#                              sm_dropdown_type = "logical",
#                              output_path = paste0("outputs/", butteR::date_file_prefix(), 
#                                                   "_combined_checks_eth_msna.xlsx"))

# create workbook ---------------------------------------------------------
# prep data
cols_to_add_to_log <- c("enum_id", "cluster_id", "point_number", "today", "admin4")

tool_support <- df_combined_log$checked_dataset %>% 
  select(uuid = `_uuid`, any_of(cols_to_add_to_log))

df_prep_checked_data <- df_combined_log$checked_dataset
df_prep_cleaning_log <- df_combined_log$cleaning_log %>%
  left_join(tool_support, by = "uuid") %>% 
  relocate(any_of(cols_to_add_to_log), .after = uuid) %>% 
  add_qn_label_to_cl(input_cl_name_col = "question",
                     input_tool = df_survey, 
                     input_tool_name_col = "name", 
                     input_tool_label_col = "label::English") %>% 
  mutate(enumerator_id = ifelse(issue %in% c("silhouette flag"), 
                                str_replace(string = str_extract(string = description, pattern = "enumerator:[0-9]{1,3}"), pattern = "enumerator:", ""),
                                enum_id))

df_prep_readme <- tibble::tribble(
  ~change_type_validation,                       ~description,
  "change_response", "Change the response to new_value",
  "blank_response",       "Remove and NA the response",
  "remove_survey",                "Delete the survey",
  "no_action",               "No action to take.")

wb_log <- createWorkbook()

hs1 <- createStyle(fgFill = "#E34443", textDecoration = "Bold", fontName = "Arial Narrow", fontColour = "white", fontSize = 12, wrapText = F)
modifyBaseFont(wb = wb_log, fontSize = 11, fontName = "Arial Narrow")

addWorksheet(wb_log, sheetName="checked_dataset")
setColWidths(wb = wb_log, sheet = "checked_dataset", cols = 1:ncol(df_prep_checked_data), widths = 24.89)
writeDataTable(wb = wb_log, sheet = "checked_dataset", 
               x = df_prep_checked_data, 
               startRow = 1, startCol = 1, 
               tableStyle = "TableStyleLight9",
               headerStyle = hs1)

# freeze pane
freezePane(wb = wb_log, "checked_dataset", firstActiveRow = 2, firstActiveCol = 2)

addWorksheet(wb_log, sheetName="cleaning_log")
setColWidths(wb = wb_log, sheet = "cleaning_log", cols = 1:ncol(df_prep_cleaning_log), widths = 24.89)
writeDataTable(wb = wb_log, sheet = "cleaning_log", 
               x = df_prep_cleaning_log, 
               startRow = 1, startCol = 1, 
               tableStyle = "TableStyleLight9",
               headerStyle = hs1)

# freeze pane
freezePane(wb = wb_log, "cleaning_log", firstActiveRow = 2, firstActiveCol = 2)

addWorksheet(wb_log, sheetName="readme")
setColWidths(wb = wb_log, sheet = "readme", cols = 1:ncol(df_prep_readme), widths = 24.89)
writeDataTable(wb = wb_log, sheet = "readme", 
               x = df_prep_readme, 
               startRow = 1, startCol = 1, 
               tableStyle = "TableStyleLight9",
               headerStyle = hs1)

# freeze pane
freezePane(wb = wb_log, "readme", firstActiveRow = 2, firstActiveCol = 2)

# openXL(wb_log)
saveWorkbook(wb_log, paste0("outputs/", butteR::date_file_prefix(),
                            "_combined_checks_eth_msna.xlsx"), overwrite = TRUE)
openXL(file = paste0("outputs/", butteR::date_file_prefix(),
                     "_combined_checks_eth_msna.xlsx"))

###############################################################################

