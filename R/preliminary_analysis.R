###############################################################################
library(tidyverse)
library(srvyr)
library(supporteR)
library(analysistools)
library(presentresults)

source("R/composite_indicators.R")

# clean data

clean_loc_data <- "inputs/ETH2403_eth_msna_cleaned_data.xlsx"

# main data
clean_data_nms <- names(readxl::read_excel(path = clean_loc_data, n_max = 5000, sheet = "cleaned_data"))
clean_c_types <- ifelse(str_detect(string = clean_data_nms, pattern = "_other$"), "text", "guess")
df_main_clean_data <- readxl::read_excel(clean_loc_data, col_types = clean_c_types, sheet = "cleaned_data") |> 
  select(-starts_with("i.")) |> 
  create_composite_indicators() |> 
  mutate(across(.cols = starts_with("i."), .fns = ~ ifelse((is.infinite(.x)|is.nan(.x)), NA, .)))

loop_support_data <- df_main_clean_data |> select(`_uuid`, admin2, i.hoh_gender, i.hoh_age, weight)

# loops
# roster
clean_data_nms_roster <- names(readxl::read_excel(path = clean_loc_data, n_max = 300, sheet = "cleaned_roster"))
clean_c_types_roster <- ifelse(str_detect(string = clean_data_nms_roster, pattern = "_other$"), "text", "guess")
df_roster_data  <- readxl::read_excel(clean_loc_data, col_types = clean_c_types_roster, sheet = "cleaned_roster") %>%
  create_composites_roster_loop()

df_clean_loop_roster <- loop_support_data %>%
  inner_join(df_roster_data, by = c("_uuid" = "_submission__uuid")) 

# education
clean_data_nms_educ <- names(readxl::read_excel(path = clean_loc_data, n_max = 300, sheet = "cleaned_educ"))
clean_c_types_educ <- ifelse(str_detect(string = clean_data_nms_educ, pattern = "_other$"), "text", "guess")
df_education_data <- readxl::read_excel(clean_loc_data, col_types = clean_c_types_educ, sheet = "cleaned_educ")

df_clean_loop_educ <- loop_support_data %>% 
  inner_join(df_education_data, by = c("_uuid" = "_submission__uuid")) 

# health
clean_data_nms_health <- names(readxl::read_excel(path = clean_loc_data, n_max = 300, sheet = "cleaned_health"))
clean_c_types_health <- ifelse(str_detect(string = clean_data_nms_health, pattern = "_other$"), "text", "guess")
df_health_clean_data <- readxl::read_excel(clean_loc_data, col_types = clean_c_types_health, sheet = "cleaned_health")

df_clean_loop_health <- loop_support_data %>%  
  inner_join(df_health_clean_data, by = c("_uuid" = "_submission__uuid")) 

# nutrition
clean_data_nms_nutr <- names(readxl::read_excel(path = clean_loc_data, n_max = 300, sheet = "cleaned_nutrition"))
clean_c_types_nutr <- ifelse(str_detect(string = clean_data_nms_nutr, pattern = "_other$"), "text", "guess")
df_nutrition_clean_data <- readxl::read_excel(clean_loc_data, col_types = clean_c_types_nutr, sheet = "cleaned_nutrition")

df_clean_loop_nutr <- loop_support_data  %>% 
  inner_join(df_nutrition_clean_data, by = c("_uuid" = "_submission__uuid")) 

# civil
clean_data_nms_civil <- names(readxl::read_excel(path = clean_loc_data, n_max = 300, sheet = "cleaned_civil"))
clean_c_types_civil <- ifelse(str_detect(string = clean_data_nms_civil, pattern = "_other$"), "text", "guess")
df_civil_clean_data <- readxl::read_excel(clean_loc_data, col_types = clean_c_types_civil, sheet = "cleaned_civil")

df_clean_loop_civil <- loop_support_data %>%  
  inner_join(df_civil_clean_data, by = c("_uuid" = "_submission__uuid")) 

# hazard conc
clean_data_nms_hazardc <- names(readxl::read_excel(path = clean_loc_data, n_max = 300, sheet = "cleaned_hazard_conc"))
clean_c_types_hazardc <- ifelse(str_detect(string = clean_data_nms_hazardc, pattern = "_other$"), "text", "guess")
df_hazard_conc_clean_data <- readxl::read_excel(clean_loc_data, col_types = clean_c_types_hazardc, sheet = "cleaned_hazard_conc")

df_clean_loop_hazardc <- loop_support_data %>% 
  inner_join(df_hazard_conc_clean_data, by = c("_uuid" = "_submission__uuid")) 

# hazard typ
clean_data_nms_hazardt <- names(readxl::read_excel(path = clean_loc_data, n_max = 300, sheet = "cleaned_hazard_typ"))
clean_c_types_hazardt <- ifelse(str_detect(string = clean_data_nms_hazardt, pattern = "_other$"), "text", "guess")
df_hazard_typ_clean_data <- readxl::read_excel(clean_loc_data, col_types = clean_c_types_hazardt, sheet = "cleaned_hazard_typ")

df_clean_loop_hazardt <- loop_support_data %>%
  inner_join(df_hazard_typ_clean_data, by = c("_uuid" = "_submission__uuid")) 

# tool
loc_tool <- "inputs/ETH2403_MSNA_2024_tool.xlsx"
df_survey <- readxl::read_excel(loc_tool, sheet = "survey")
df_choices <- readxl::read_excel(loc_tool, sheet = "choices")
 
# loa // list of analysis
all_loa_msna <- read_csv("inputs/r_loa_msna_eth.csv")

# individual to hh level ------------------------------------------------------

list_individual_to_hh <- list()

# hh_with_disabled_member
df_hh_with_disabled_member <- df_clean_loop_roster %>% 
  create_composites_roster_loop() %>% 
  group_by(`_uuid`) %>% 
  summarise(int.hh_disability_status = paste(i.disability, collapse = " : ")) %>% 
  mutate(i.hh_with_disabled_member = case_when(str_detect(string = int.hh_disability_status, pattern = "yes") ~ "yes_disability",
                                               !str_detect(string = int.hh_disability_status, pattern = "yes") ~ "no_disability")) %>% 
  select(-int.hh_disability_status)

add_checks_data_to_list(input_list_name = "list_individual_to_hh", input_df_name = "df_hh_with_disabled_member")

# combine the calculated indicators
df_combined_hh_indicators_from_roster <- list_individual_to_hh %>%
  reduce(.f = full_join, by = '_uuid')

# data with composites --------------------------------------------------------
# main data with composites
df_data_with_composites <- df_main_clean_data %>% 
  left_join(df_combined_hh_indicators_from_roster, by = c("_uuid" = "_uuid")) 

#  analysis - main ------------------------------------------------------------
# main
df_main <- df_main_clean_data 

# survey object
main_svy <- as_survey(.data = df_main, strata = strata)

# loa
df_main_loa <- all_loa_msna %>% 
  filter(dataset %in% c("main_data"))

# analysis
df_main_analysis<- analysistools::create_analysis(design = main_svy, 
                                                   loa = df_main_loa, sm_separator = "/")

# analysis - roster -----------------------------------------------------------
# roster
df_roster <- df_clean_loop_roster 

# survey object
roster_svy <- as_survey(.data = df_roster)

# loa roster
df_roster_loa <- all_loa_msna %>% 
  filter(dataset %in% c("roster"))

# analysis
df_roster_analysis <- analysistools::create_analysis(design = roster_svy, 
                                                     loa = df_roster_loa, sm_separator = "/")

# loops analysis - educ -------------------------------------------------------
# education
df_education <- df_clean_loop_educ 

# survey object - education
education_svy <- as_survey(.data = df_education)

# loa education
df_education_loa <- all_loa_msna %>% 
  filter(dataset %in% c("educ_ind"))

# analysis
df_education_analysis <- analysistools::create_analysis(design = education_svy, 
                                                        loa = df_education_loa, sm_separator = "/")

# loops analysis - health ----------------------------------------------------
# health
df_health <- df_clean_loop_health 

# survey object - health
health_svy <- as_survey(.data = df_health)

# loa health
df_health_loa <- all_loa_msna %>% 
  filter(dataset %in% c("health_ind"))

# analysis
df_health_analysis <- analysistools::create_analysis(design = health_svy, 
                                                     loa = df_health_loa, sm_separator = "/")

# loops analysis - nutrition --------------------------------------------------
# nutrition
df_nutrition <- df_clean_loop_nutr 

# survey object - nutrition
nutrition_svy <- as_survey(.data = df_nutrition)

# loa nutrition
df_nutrition_loa <- all_loa_msna %>% 
  filter(dataset %in% c("nutr_ind"))

# analysis
df_nutrition_analysis <- analysistools::create_analysis(design = nutrition_svy, 
                                                        loa = df_nutrition_loa, sm_separator = "/")

# loops analysis - civil ------------------------------------------------------
# civil
df_civil <- df_clean_loop_civil 

# survey object - civil
civil_svy <- as_survey(.data = df_civil)

# loa civil
df_civil_loa <- all_loa_msna %>% 
  filter(dataset %in% c("civil_ind"))

# analysis
df_civil_analysis <- analysistools::create_analysis(design = civil_svy, 
                                                    loa = df_civil_loa, sm_separator = "/")

# loops analysis - hazardc ----------------------------------------------------
# hazard conc
df_hazardc <- df_clean_loop_hazardc 

# survey object - hazard conc
hazardc_svy <- as_survey(.data = df_hazardc)

# loa hazard conc
df_hazardc_loa <- all_loa_msna %>% 
  filter(dataset %in% c("hazardc_ind"))

# analysis
df_hazardc_analysis <- analysistools::create_analysis(design = hazardc_svy, 
                                                     loa = df_hazardc_loa, sm_separator = "/")

# loops analysis - hazardt ----------------------------------------------------
# hazard typ
df_hazardt  <- df_clean_loop_hazardt 

# survey object - hazard typ
hazardt_svy <- as_survey(.data = df_hazardt)

# loa hazard typ
df_hazardt_loa <- all_loa_msna %>% 
  filter(dataset %in% c("hazardt_ind"))

# analysis
df_hazardt_analysis <- analysistools::create_analysis(design = hazardt_svy, 
                                                      loa = df_hazardt_loa, sm_separator = "/")

# analysis tables -------------------------------------------------------------

# combine the tables

df_combined_tables <- bind_rows(df_main_analysis$results_table,
                                df_roster_analysis$results_table,
                                df_education_analysis$results_table,
                                df_health_analysis$results_table,
                                df_nutrition_analysis$results_table,
                                df_civil_analysis$results_table,
                                df_hazardc_analysis$results_table,
                                df_hazardt_analysis$results_table)

df_analysis_table <- presentresults::create_table_variable_x_group(results_table = df_combined_tables) %>% 
  filter(!(analysis_type %in% c("prop_select_one", "prop_select_multiple") & (is.na(analysis_var_value) | analysis_var_value %in% c("NA"))))

presentresults::create_xlsx_variable_x_group(table_group_x_variable = df_analysis_table,
                                             file_path = paste0("outputs/", butteR::date_file_prefix(), 
                                                                "_ETH2403_MSNA_analysis_tables.xlsx"),
                                             table_sheet_name = "msna", overwrite = TRUE)
###############################################################################                                             
