###############################################################################
# Applying the cleaning log to clean the data

rm(list = ls())

library(tidyverse)
library(glue)
library(cleaningtools)
library(httr)
library(supporteR)

loc_data <- "inputs/ETH2403_MSNA_2024_data_all.xlsx"

# main data
data_nms <- names(readxl::read_excel(path = loc_data, n_max = 300))
c_types <- ifelse(str_detect(string = data_nms, pattern = "_other$"), "text", "guess")
df_tool_data <- readxl::read_excel(loc_data, col_types = c_types) %>% 
  mutate(start = as_datetime(start),
         end = as_datetime(end),
         enumerator_id = ifelse(is.na(enum_id), enum_id, enum_id)) %>% 
  checks_add_extra_cols(input_enumerator_id_col = "enumerator_id",
                        input_location_col = "admin4")

# loops
# roster
data_nms_roster <- names(readxl::read_excel(path = loc_data, n_max = 300, sheet = "roster"))
c_types_roster <- ifelse(str_detect(string = data_nms_roster, pattern = "_other$"), "text", "guess")
df_loop_roster <- readxl::read_excel(loc_data, col_types = c_types_roster, sheet = "roster")

# education
data_nms_educ <- names(readxl::read_excel(path = loc_data, n_max = 300, sheet = "edu_ind"))
c_types_educ <- ifelse(str_detect(string = data_nms_educ, pattern = "_other$"), "text", "guess")
df_loop_educ <- readxl::read_excel(loc_data, col_types = c_types_educ, sheet = "edu_ind")

# health
data_nms_health <- names(readxl::read_excel(path = loc_data, n_max = 300, sheet = "health_ind"))
c_types_health <- ifelse(str_detect(string = data_nms_health, pattern = "_other$"), "text", "guess")
df_loop_health <- readxl::read_excel(loc_data, col_types = c_types_health, sheet = "health_ind")

# civil
data_nms_civil <- names(readxl::read_excel(path = loc_data, n_max = 300, sheet = "civil_ind"))
c_types_civil <- ifelse(str_detect(string = data_nms_civil, pattern = "_other$"), "text", "guess")
df_loop_civil <- readxl::read_excel(loc_data, col_types = c_types_civil, sheet = "civil_ind")

# hazard_concern
data_nms_hazard_conc <- names(readxl::read_excel(path = loc_data, n_max = 300, sheet = "hazard_concern_rep"))
c_types_hazard_conc <- ifelse(str_detect(string = data_nms_hazard_conc, pattern = "_other$"), "text", "guess")
df_loop_hazard_conc <- readxl::read_excel(loc_data, col_types = c_types_hazard_conc, sheet = "hazard_concern_rep")

# hazard_type
data_nms_hazard_type <- names(readxl::read_excel(path = loc_data, n_max = 300, sheet = "hazard_type_rep"))
c_types_hazard_type <- ifelse(str_detect(string = data_nms_hazard_type, pattern = "_other$"), "text", "guess")
df_loop_hazard_type <- readxl::read_excel(loc_data, col_types = c_types_hazard_type, sheet = "hazard_type_rep")

# nutrition
data_nms_nut <- names(readxl::read_excel(path = loc_data, n_max = 300, sheet = "nut_ind"))
c_types_nut <- ifelse(str_detect(string = data_nms_nut, pattern = "_other$"), "text", "guess")
df_loop_nut <- readxl::read_excel(loc_data, col_types = c_types_nut, sheet = "nut_ind")

# tool
loc_tool <- "inputs/ETH2403_MSNA_2024_tool.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")

df_filled_cl <- readxl::read_excel("inputs/main_combined_checks_eth_msna_gg.xlsx", sheet = "cleaning_log") %>% 
  filter(!is.na(reviewed), !question %in% c("_index"), !uuid %in% c("all"))

df_remove_survey_cl <- df_filled_cl %>% 
  filter(change_type %in% c("remove_survey"))

# check pii ---------------------------------------------------------------
pii_from_data_host <- cleaningtools::check_pii(dataset = df_tool_data, element_name = "checked_dataset", uuid_column = "_uuid")
pii_from_data_host$potential_PII

# then determine which columns to remove from both the raw and clean data
cols_to_remove <- c("audit", "audit_URL", "pt_num_msg", "pt_num_validation_message",
                    "pt_sample_lat", "pt_sample_lon", "dist_btn_sample_collected", 
                    "threshold_msg_2_positive", "threshold_msg_2_negative",
                    "telephone", "contact", "name", "instance_name", "household_geopoint", 
                    "_household_geopoint_latitude",	"_household_geopoint_longitude",	
                    "_household_geopoint_altitude",	"_household_geopoint_precision")

# Main dataset ------------------------------------------------------------

# filtered log
df_filled_cl_main <- df_filled_cl %>% 
  filter(is.na(sheet))

# updating the main dataset with new columns

df_data_with_added_cols <- cts_add_new_sm_choices_to_data(input_df_tool_data = df_tool_data,
                                                               input_df_filled_cl = df_filled_cl_main, 
                                                               input_df_survey = df_survey,
                                                               input_df_choices = df_choices)

# check the cleaning log
df_cl_review <- cleaningtools::review_cleaning_log(
  raw_dataset = df_data_with_added_cols,
  raw_data_uuid_column = "_uuid",
  cleaning_log = df_filled_cl_main,
  cleaning_log_change_type_column = "change_type",
  change_response_value = "change_response",
  cleaning_log_question_column = "question",
  cleaning_log_uuid_column = "uuid",
  cleaning_log_new_value_column = "new_value")

# filter log for cleaning
df_final_cleaning_log <- df_filled_cl_main %>% 
  filter(!question %in% c("duration_audit_sum_all_ms", "duration_audit_sum_all_minutes", "phone_consent"), 
         !uuid %in% c("all")) %>% 
  filter(!str_detect(string = question, pattern = "\\w+\\/$"))

# create the clean data from the raw data and cleaning log
df_cleaning_step <- cleaningtools::create_clean_data(
  raw_dataset = df_data_with_added_cols %>% select(-any_of(cols_to_remove)),
  raw_data_uuid_column = "_uuid",
  cleaning_log = df_final_cleaning_log,
  cleaning_log_change_type_column = "change_type",
  change_response_value = "change_response",
  NA_response_value = "blank_response",
  no_change_value = "no_action",
  remove_survey_value = "remove_survey",
  cleaning_log_question_column = "question",
  cleaning_log_uuid_column = "uuid",
  cleaning_log_new_value_column = "new_value")

# handle parent question columns
df_updating_sm_parents <- cts_update_sm_parent_cols(input_df_cleaning_step_data = df_cleaning_step, 
                                                         input_uuid_col = "_uuid",
                                                         input_point_id_col = "point_number",
                                                         input_collected_date_col = "today",
                                                         input_location_col = "admin4")

# tool data to support loops ----------------------------------------------

df_tool_support_data_for_loops <- df_updating_sm_parents$updated_sm_parents %>% 
  filter(!`_uuid` %in% df_remove_survey_cl$uuid) %>% 
  select(`_uuid`, admin4, today, enumerator_id, point_number)

# roster cleaning ---------------------------------------------------------

# then determine wich columns to remove from both the raw and clean data
cols_to_remove_roster <- c("name")
# filtered log
df_filled_cl_roster <- df_filled_cl %>% 
  filter(sheet %in% c("roster", "grp_hh_information"), !is.na(index))

# updating the main dataset with new columns

df_data_with_added_cols_roster <- cts_add_new_sm_choices_to_data(input_df_tool_data = df_loop_roster %>% 
                                                                        left_join(df_tool_support_data_for_loops, by = c("_submission__uuid" = "_uuid")),
                                                                      input_df_filled_cl = df_filled_cl_roster, 
                                                                      input_df_survey = df_survey,
                                                                      input_df_choices = df_choices)

# check the cleaning log
df_cl_review <- cleaningtools::review_cleaning_log(
  raw_dataset = df_data_with_added_cols_roster,
  raw_data_uuid_column = "_submission__uuid",
  cleaning_log = df_filled_cl_roster,
  cleaning_log_change_type_column = "change_type",
  change_response_value = "change_response",
  cleaning_log_question_column = "question",
  cleaning_log_uuid_column = "uuid",
  cleaning_log_new_value_column = "new_value")

# filter log for cleaning
df_final_cleaning_log_roster <- df_filled_cl_roster %>% 
  filter(!question %in% c("duration_audit_sum_all_ms", "duration_audit_sum_all_minutes", "phone_consent",
                          "_index", "_parent_index"), 
         !uuid %in% c("all")) %>% 
  filter(!str_detect(string = question, pattern = "\\w+\\/$"))

# create the clean data from the raw data and cleaning log
df_cleaning_step_roster <- cleaningtools::create_clean_data(
  raw_dataset = df_data_with_added_cols_roster %>% 
    select(-any_of(cols_to_remove_roster)) %>% 
    mutate(cleaning_uuid = paste0(`_submission__uuid`, `_index`)),
  raw_data_uuid_column = "cleaning_uuid",
  cleaning_log = df_final_cleaning_log_roster %>% 
    mutate(log_cleaning_uuid = paste0(uuid, index)),
  cleaning_log_change_type_column = "change_type",
  change_response_value = "change_response",
  NA_response_value = "blank_response",
  no_change_value = "no_action",
  remove_survey_value = "remove_survey",
  cleaning_log_question_column = "question",
  cleaning_log_uuid_column = "log_cleaning_uuid",
  cleaning_log_new_value_column = "new_value")

# handle parent question columns
df_updating_sm_parents_roster <- cts_update_sm_parent_cols(input_df_cleaning_step_data = df_cleaning_step_roster,
                                                                input_uuid_col = "_submission__uuid",
                                                                input_enumerator_id_col = "enumerator_id",
                                                                input_point_id_col = "point_number",
                                                                input_collected_date_col = "today",
                                                                input_location_col = "admin4", 
                                                                input_dataset_type = "loop", 
                                                                input_sheet_name = "roster", 
                                                                input_index_col = "_index")

# education cleaning ---------------------------------------------------------
# then determine which columns to remove from both the raw and clean data
cols_to_remove_educ <- c("name")
# filtered log
df_filled_cl_educ <- df_filled_cl %>% 
  filter(sheet %in% c("edu_ind", "grp_edu"), !is.na(index))

# check the cleaning log
df_cl_review <- cleaningtools::review_cleaning_log(raw_dataset = df_loop_educ,
                                                   raw_data_uuid_column = "_submission__uuid",
                                                   cleaning_log = df_filled_cl_educ,
                                                   cleaning_log_change_type_column = "change_type",
                                                   change_response_value = "change_response",
                                                   cleaning_log_question_column = "question",
                                                   cleaning_log_uuid_column = "uuid",
                                                   cleaning_log_new_value_column = "new_value")

# filter log for cleaning
df_final_cleaning_log_educ <- df_filled_cl_educ %>% 
  filter(!question %in% c("duration_audit_sum_all_ms", "duration_audit_sum_all_minutes", "phone_consent",
                          "_index", "_parent_index"), 
         !uuid %in% c("all")) %>% 
  filter(!str_detect(string = question, pattern = "\\w+\\/$"))

# create the clean data from the raw data and cleaning log
df_cleaning_step_educ <- cleaningtools::create_clean_data(raw_dataset = df_loop_educ %>% 
                                                            mutate(cleaning_uuid = paste0(`_submission__uuid`, `_index`)),
                                                          raw_data_uuid_column = "cleaning_uuid",
                                                          cleaning_log = df_final_cleaning_log_educ %>% 
                                                            mutate(log_cleaning_uuid = paste0(uuid, index)),
                                                          cleaning_log_change_type_column = "change_type",
                                                          change_response_value = "change_response",
                                                          NA_response_value = "blank_response",
                                                          no_change_value = "no_action",
                                                          remove_survey_value = "remove_survey",
                                                          cleaning_log_question_column = "question",
                                                          cleaning_log_uuid_column = "log_cleaning_uuid",
                                                          cleaning_log_new_value_column = "new_value")

# health cleaning ---------------------------------------------------------
# then determine which columns to remove from both the raw and clean data
cols_to_remove_health <- c("name")
# filtered log
df_filled_cl_health <- df_filled_cl %>% 
  filter(sheet %in% c("health_ind", "grp_health"), !is.na(index))

# check the cleaning log
df_cl_review <- cleaningtools::review_cleaning_log(raw_dataset = df_loop_health,
                                                   raw_data_uuid_column = "_submission__uuid",
                                                   cleaning_log = df_filled_cl_health,
                                                   cleaning_log_change_type_column = "change_type",
                                                   change_response_value = "change_response",
                                                   cleaning_log_question_column = "question",
                                                   cleaning_log_uuid_column = "uuid",
                                                   cleaning_log_new_value_column = "new_value")

# filter log for cleaning
df_final_cleaning_log_health <- df_filled_cl_health %>% 
  filter(!question %in% c("duration_audit_sum_all_ms", "duration_audit_sum_all_minutes", "phone_consent",
                          "_index", "_parent_index"), 
         !uuid %in% c("all")) %>% 
  filter(!str_detect(string = question, pattern = "\\w+\\/$"))

# create the clean data from the raw data and cleaning log
df_cleaning_step_health <- cleaningtools::create_clean_data(raw_dataset = df_loop_health %>% 
                                                              mutate(cleaning_uuid = paste0(`_submission__uuid`, `_index`)),
                                                            raw_data_uuid_column = "cleaning_uuid",
                                                            cleaning_log = df_final_cleaning_log_health %>% 
                                                              mutate(log_cleaning_uuid = paste0(uuid, index)),
                                                            cleaning_log_change_type_column = "change_type",
                                                            change_response_value = "change_response",
                                                            NA_response_value = "blank_response",
                                                            no_change_value = "no_action",
                                                            remove_survey_value = "remove_survey",
                                                            cleaning_log_question_column = "question",
                                                            cleaning_log_uuid_column = "log_cleaning_uuid",
                                                            cleaning_log_new_value_column = "new_value")
# civil cleaning ---------------------------------------------------------
# then determine which columns to remove from both the raw and clean data
cols_to_remove_civil <- c("name")
# filtered log
df_filled_cl_civil <- df_filled_cl %>% 
  filter(sheet %in% c("civil_ind", "civil_ind"), !is.na(index))

# check the cleaning log
df_cl_review <- cleaningtools::review_cleaning_log(raw_dataset = df_loop_civil,
                                                   raw_data_uuid_column = "_submission__uuid",
                                                   cleaning_log = df_filled_cl_civil,
                                                   cleaning_log_change_type_column = "change_type",
                                                   change_response_value = "change_response",
                                                   cleaning_log_question_column = "question",
                                                   cleaning_log_uuid_column = "uuid",
                                                   cleaning_log_new_value_column = "new_value")

# filter log for cleaning
df_final_cleaning_log_civil <- df_filled_cl_civil %>% 
  filter(!question %in% c("duration_audit_sum_all_ms", "duration_audit_sum_all_minutes", "phone_consent",
                          "_index", "_parent_index"), 
         !uuid %in% c("all")) %>% 
  filter(!str_detect(string = question, pattern = "\\w+\\/$"))

# create the clean data from the raw data and cleaning log
df_cleaning_step_civil <- cleaningtools::create_clean_data(raw_dataset = df_loop_civil %>% 
                                                             mutate(cleaning_uuid = paste0(`_submission__uuid`, `_index`)),
                                                           raw_data_uuid_column = "cleaning_uuid",
                                                           cleaning_log = df_final_cleaning_log_civil %>% 
                                                             mutate(log_cleaning_uuid = paste0(uuid, index)),
                                                           cleaning_log_change_type_column = "change_type",
                                                           change_response_value = "change_response",
                                                           NA_response_value = "blank_response",
                                                           no_change_value = "no_action",
                                                           remove_survey_value = "remove_survey",
                                                           cleaning_log_question_column = "question",
                                                           cleaning_log_uuid_column = "log_cleaning_uuid",
                                                           cleaning_log_new_value_column = "new_value")

# hazard concern cleaning -----------------------------------------------------
# then determine which columns to remove from both the raw and clean data
cols_to_remove_hazardc <- c("name")
# filtered log
df_filled_cl_hazardc <- df_filled_cl %>% 
  filter(sheet %in% c("hazard_concern_rep", "hazard_concern_rep"), !is.na(index))

# check the cleaning log
df_cl_review <- cleaningtools::review_cleaning_log(raw_dataset = df_loop_hazard_conc,
                                                   raw_data_uuid_column = "_submission__uuid",
                                                   cleaning_log = df_filled_cl_hazardc,
                                                   cleaning_log_change_type_column = "change_type",
                                                   change_response_value = "change_response",
                                                   cleaning_log_question_column = "question",
                                                   cleaning_log_uuid_column = "uuid",
                                                   cleaning_log_new_value_column = "new_value")

# filter log for cleaning
df_final_cleaning_log_hazardc <- df_filled_cl_hazardc %>% 
  filter(!question %in% c("duration_audit_sum_all_ms", "duration_audit_sum_all_minutes", "phone_consent",
                          "_index", "_parent_index"), 
         !uuid %in% c("all")) %>% 
  filter(!str_detect(string = question, pattern = "\\w+\\/$"))

# create the clean data from the raw data and cleaning log
df_cleaning_step_hazardc <- cleaningtools::create_clean_data(raw_dataset = df_loop_hazard_conc %>% 
                                                               mutate(cleaning_uuid = paste0(`_submission__uuid`, `_index`)),
                                                             raw_data_uuid_column = "cleaning_uuid",
                                                             cleaning_log = df_final_cleaning_log_hazardc %>% 
                                                               mutate(log_cleaning_uuid = paste0(uuid, index)),
                                                             cleaning_log_change_type_column = "change_type",
                                                             change_response_value = "change_response",
                                                             NA_response_value = "blank_response",
                                                             no_change_value = "no_action",
                                                             remove_survey_value = "remove_survey",
                                                             cleaning_log_question_column = "question",
                                                             cleaning_log_uuid_column = "log_cleaning_uuid",
                                                             cleaning_log_new_value_column = "new_value")

# hazard type cleaning --------------------------------------------------------
# then determine which columns to remove from both the raw and clean data
cols_to_remove_hazardt <- c("name")
# filtered log
df_filled_cl_hazardt <- df_filled_cl %>% 
  filter(sheet %in% c("hazard_type_rep", "hazard_type_rep"), !is.na(index))

# check the cleaning log
df_cl_review <- cleaningtools::review_cleaning_log(raw_dataset = df_loop_hazard_type,
                                                   raw_data_uuid_column = "_submission__uuid",
                                                   cleaning_log = df_filled_cl_hazardt,
                                                   cleaning_log_change_type_column = "change_type",
                                                   change_response_value = "change_response",
                                                   cleaning_log_question_column = "question",
                                                   cleaning_log_uuid_column = "uuid",
                                                   cleaning_log_new_value_column = "new_value")

# filter log for cleaning
df_final_cleaning_log_hazardt <- df_filled_cl_hazardt %>% 
  filter(!question %in% c("duration_audit_sum_all_ms", "duration_audit_sum_all_minutes", "phone_consent",
                          "_index", "_parent_index"), 
         !uuid %in% c("all")) %>% 
  filter(!str_detect(string = question, pattern = "\\w+\\/$"))

# create the clean data from the raw data and cleaning log
df_cleaning_step_hazardt <- cleaningtools::create_clean_data(raw_dataset = df_loop_hazard_type %>% 
                                                               mutate(cleaning_uuid = paste0(`_submission__uuid`, `_index`)),
                                                             raw_data_uuid_column = "cleaning_uuid",
                                                             cleaning_log = df_final_cleaning_log_hazardt %>% 
                                                               mutate(log_cleaning_uuid = paste0(uuid, index)),
                                                             cleaning_log_change_type_column = "change_type",
                                                             change_response_value = "change_response",
                                                             NA_response_value = "blank_response",
                                                             no_change_value = "no_action",
                                                             remove_survey_value = "remove_survey",
                                                             cleaning_log_question_column = "question",
                                                             cleaning_log_uuid_column = "log_cleaning_uuid",
                                                             cleaning_log_new_value_column = "new_value")

# nutrition cleaning ---------------------------------------------------------
# then determine which columns to remove from both the raw and clean data
cols_to_remove_nut <- c("name")
# filtered log
df_filled_cl_nut <- df_filled_cl %>% 
  filter(sheet %in% c("nut_ind", "grp_nutrition"), !is.na(index))

# check the cleaning log
df_cl_review <- cleaningtools::review_cleaning_log(raw_dataset = df_loop_nut,
                                                   raw_data_uuid_column = "_submission__uuid",
                                                   cleaning_log = df_filled_cl_nut,
                                                   cleaning_log_change_type_column = "change_type",
                                                   change_response_value = "change_response",
                                                   cleaning_log_question_column = "question",
                                                   cleaning_log_uuid_column = "uuid",
                                                   cleaning_log_new_value_column = "new_value")

# filter log for cleaning
df_final_cleaning_log_nut <- df_filled_cl_nut %>% 
  filter(!question %in% c("duration_audit_sum_all_ms", "duration_audit_sum_all_minutes", "phone_consent",
                          "_index", "_parent_index"), 
         !uuid %in% c("all")) %>% 
  filter(!str_detect(string = question, pattern = "\\w+\\/$"))

# create the clean data from the raw data and cleaning log
df_cleaning_step_nut <- cleaningtools::create_clean_data(raw_dataset = df_loop_nut %>% 
                                                           mutate(cleaning_uuid = paste0(`_submission__uuid`, `_index`)),
                                                         raw_data_uuid_column = "cleaning_uuid",
                                                         cleaning_log = df_final_cleaning_log_nut %>% 
                                                           mutate(log_cleaning_uuid = paste0(uuid, index)),
                                                         cleaning_log_change_type_column = "change_type",
                                                         change_response_value = "change_response",
                                                         NA_response_value = "blank_response",
                                                         no_change_value = "no_action",
                                                         remove_survey_value = "remove_survey",
                                                         cleaning_log_question_column = "question",
                                                         cleaning_log_uuid_column = "log_cleaning_uuid",
                                                         cleaning_log_new_value_column = "new_value")

# export datasets ---------------------------------------------------------

list_of_datasets <- list("raw_data" = df_tool_data %>% select(-any_of(cols_to_remove)),
                              "raw_roster" = df_loop_roster %>% select(-any_of(cols_to_remove_roster)),
                              "raw_educ" = df_loop_educ,
                              "raw_health" = df_loop_health,
                              "raw_nutrition" = df_loop_nut,
                              "raw_civil" = df_loop_civil,
                              "raw_hazardc" = df_loop_hazard_conc,
                              "raw_hazardt" = df_loop_hazard_type,
                              "cleaned_data" = df_updating_sm_parents$updated_sm_parents %>% 
                                filter(!`_uuid` %in% df_remove_survey_cl$uuid),
                              "cleaned_roster" = df_loop_roster %>% 
                                filter(!`_submission__uuid` %in% df_remove_survey_cl$uuid), #%>% 
                                #select(-cleaning_uuid),
                              "cleaned_educ" = df_cleaning_step_educ %>% 
                                filter(!`_submission__uuid` %in% df_remove_survey_cl$uuid) %>% 
                                select(-cleaning_uuid),
                              "cleaned_health" = df_cleaning_step_health %>% 
                                filter(!`_submission__uuid` %in% df_remove_survey_cl$uuid) %>% 
                                select(-cleaning_uuid),
                              "cleaned_nutrition" = df_loop_nut %>% 
                                filter(!`_submission__uuid` %in% df_remove_survey_cl$uuid), #%>%
                                # select(-cleaning_uuid)
                              "cleaned_civil" = df_loop_civil %>% 
                                filter(!`_submission__uuid` %in% df_remove_survey_cl$uuid), #%>%
                                # select(-cleaning_uuid),
                              "cleaned_hazardc" = df_loop_hazard_conc %>% 
                                filter(!`_submission__uuid` %in% df_remove_survey_cl$uuid), #%>%
                                #  select(-cleaning_uuid),
                              "cleaned_hazardt" = df_loop_hazard_type %>% 
                                filter(!`_submission__uuid` %in% df_remove_survey_cl$uuid) #%>%
                                #  select(-cleaning_uuid)
                              )

openxlsx::write.xlsx(list_of_datasets, paste0("outputs/", butteR::date_file_prefix(), 
                                              "_ETH2403_eth_msna_cleaned_data.xlsx"), overwrite = TRUE)

# extra log for recreated select multiple ---------------------------------

list_of_extra_logs <- list("extra_log_sm_parents" = df_updating_sm_parents$extra_log_sm_parents,
                           "extra_log_sm_parents_roster" = df_updating_sm_parents_roster$extra_log_sm_parents)

openxlsx::write.xlsx(list_of_extra_logs, 
                     paste0("outputs/", butteR::date_file_prefix(), 
                            "_extra_sm_parent_changes_checks_eth_msna.xlsx"), overwrite = TRUE)

###############################################################################
