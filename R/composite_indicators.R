###############################################################################
# creating composite indicators -----------------------------------------------

create_composite_indicators <- function(input_df) {
  input_df |> 
    dplyr::mutate(int.fcs_cereals_tubers = fsl_fcs_cereal*2,
           int.fcs_pulses = fsl_fcs_legumes*3,
           int.fcs_vegetables = fsl_fcs_veg,
           int.fcs_fruit = fsl_fcs_fruit,
           int.fcs_meat_fish = fsl_fcs_meat*4,
           int.fcs_dairy = fsl_fcs_dairy*4,
           int.fcs_sugar = fsl_fcs_sugar*0.5,
           int.fcs_oils = fsl_fcs_oil*0.5,
           int.rCSILessQlty = fsl_rcsi_lessquality,
           int.rCSIBorrow = 2 * fsl_rcsi_borrow,
           int.rCSIMealSize = fsl_rcsi_mealsize,
           int.rCSIMealAdult = 3 * fsl_rcsi_mealadult,
           int.rCSIMealNb = fsl_rcsi_mealnb,
           int.freq_no_food_lack_resources = case_when(fsl_hhs_nofoodh %in% c("no") ~ 0,
                                                       fsl_hhs_nofoodh %in% c("yes") & fsl_hhs_nofoodhh_freq %in% c("rarely") ~ 0,
                                                       fsl_hhs_nofoodh %in% c("yes") & fsl_hhs_nofoodhh_freq %in% c("sometimes") ~ 1,
                                                       fsl_hhs_nofoodh %in% c("yes") & fsl_hhs_nofoodhh_freq %in% c("often") ~ 2),
           int.freq_sleep_hungry = case_when(fsl_hhs_sleephungry %in% c("no") ~ 0,
                                             fsl_hhs_sleephungry %in% c("yes") & fsl_hhs_sleephungry_freq %in% c("rarely") ~ 0,
                                             fsl_hhs_sleephungry %in% c("yes") & fsl_hhs_sleephungry_freq %in% c("sometimes") ~ 1,
                                             fsl_hhs_sleephungry %in% c("yes") & fsl_hhs_sleephungry_freq %in% c("often") ~ 2),
           int.freq_day_and_night_no_food = case_when(fsl_hhs_alldaynight %in% c("no") ~ 0,
                                                      fsl_hhs_alldaynight %in% c("yes") & fsl_hhs_alldaynight_freq %in% c("rarely") ~ 0,
                                                      fsl_hhs_alldaynight %in% c("yes") & fsl_hhs_alldaynight_freq %in% c("sometimes") ~ 1,
                                                      fsl_hhs_alldaynight %in% c("yes") & fsl_hhs_alldaynight_freq %in% c("often") ~ 2) 
    ) |> 
    rowwise() |> 
    dplyr::mutate(int.fcs = sum(c_across(int.fcs_cereals_tubers:int.fcs_oils)),
           int.rcsi = sum(c_across(int.rCSILessQlty:int.rCSIMealNb)),
           int.hhs = sum(c_across(int.freq_no_food_lack_resources:int.freq_day_and_night_no_food)),
           i.hh_size = case_when(hh_size <= 3 ~ "between_1_and_3_members",
                                 hh_size <= 6 ~ "between_4_and_6_members",
                                 hh_size <= 9 ~ "between_7_and_9_members",
                                 hh_size >= 10 ~ "10_or_more_members"),
           i.respondent_age = case_when(resp_age <= 24 ~ "age_18_24",
                                        resp_age <= 39 ~ "age_25_39",
                                        resp_age <= 59 ~ "age_40_59",
                                        resp_age > 59 ~ "age_60+"),
           #int.hh_number_male = sum(c_across(c("hh_number_men_count", "hh_number_boys_count")), na.rm = T),
           #int.hh_number_female = sum(c_across(c("hh_number_women_count", "hh_number_girls_count")), na.rm = T)
           
    ) |>
    ungroup() |>
    dplyr::mutate(i.fcs = int.fcs,
           i.fcs_cat = case_when(i.fcs <= 21 ~ "Poor",
                                 i.fcs <= 35 ~ "Borderline",
                                 i.fcs <= 112 ~ "Acceptable"),
           i.rcsi = int.rcsi,
           i.rcsi_cat = case_when(i.rcsi < 4 ~ "rcsi_0_3",
                                  i.rcsi < 19 ~ "rcsi_4_18",
                                  i.rcsi >= 19 ~ "rcsi_19+"),
           i.hhs = int.hhs,
           i.hhs_cat = case_when(i.hhs == 0 ~ "None",
                                 i.hhs == 1 ~ "Slight",
                                 i.hhs <= 3 ~ "Moderate",
                                 i.hhs == 4 ~ "Severe",
                                 i.hhs <= 6 ~ "Very severe"),
           
           #i.hh_composition_size = int.hh_size,
           i.hoh_gender = ifelse(is.na(hoh_gender), resp_gender, hoh_gender),
           i.hoh_age = ifelse(is.na(hoh_age), resp_age, hoh_age),
           
           i.fc_matrix = case_when( 
             # 1 - 5
             i.hhs == 0 & i.rcsi < 4 & i.fcs > 35 ~ 1,
             i.hhs == 1 & i.rcsi < 4 & i.fcs > 35 ~ 2,
             (i.hhs >= 2 & i.hhs <= 3) & i.rcsi < 4 & i.fcs > 35 ~ 3,
             i.hhs == 4 & i.rcsi < 4 & i.fcs > 35 ~ 4,
             (i.hhs >= 5 & i.hhs <= 6) & i.rcsi < 4 & i.fcs > 35 ~ 5,
             # 6 - 10
             i.hhs == 0 & i.rcsi < 4 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 6,
             i.hhs == 1 & i.rcsi < 4 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 7,
             (i.hhs >= 2 & i.hhs <= 3) & i.rcsi < 4 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 8,
             i.hhs == 4 & i.rcsi < 4 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 9,
             (i.hhs >= 5 & i.hhs <= 6) & i.rcsi < 4 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 10,
             # 11 - 15
             i.hhs == 0 & i.rcsi < 4 & i.fcs  < 21.5 ~ 11,
             i.hhs == 1 & i.rcsi < 4 & i.fcs  < 21.5 ~ 12,
             (i.hhs >= 2 & i.hhs <= 3) & i.rcsi < 4 & i.fcs  < 21.5 ~ 13,
             i.hhs == 4 & i.rcsi < 4 & i.fcs  < 21.5 ~ 14,
             (i.hhs >= 5 & i.hhs <= 6) & i.rcsi < 4 & i.fcs  < 21.5 ~ 15,
             # 16 - 20
             i.hhs == 0 & (i.rcsi >= 4 & i.rcsi <= 18) & i.fcs > 35 ~ 16,
             i.hhs == 1 & (i.rcsi >= 4 & i.rcsi <= 18) & i.fcs > 35 ~ 17,
             (i.hhs >= 2 & i.hhs <= 3) & (i.rcsi >= 4 & i.rcsi <= 18) & i.fcs > 35 ~ 18,
             i.hhs == 4 & (i.rcsi >= 4 & i.rcsi <= 18) & i.fcs > 35 ~ 19,
             (i.hhs >= 5 & i.hhs <= 6) & (i.rcsi >= 4 & i.rcsi <= 18) & i.fcs > 35 ~ 20,
             # 21 - 25
             i.hhs == 0 & (i.rcsi >= 4 & i.rcsi <= 18) & (i.fcs >= 21.5 & i.fcs <= 35) ~ 21,
             i.hhs == 1 & (i.rcsi >= 4 & i.rcsi <= 18) & (i.fcs >= 21.5 & i.fcs <= 35) ~ 22,
             (i.hhs >= 2 & i.hhs <= 3) & (i.rcsi >= 4 & i.rcsi <= 18) & (i.fcs >= 21.5 & i.fcs <= 35) ~ 23,
             i.hhs == 4 & (i.rcsi >= 4 & i.rcsi <= 18) & (i.fcs >= 21.5 & i.fcs <= 35) ~ 24,
             (i.hhs >= 5 & i.hhs <= 6) & (i.rcsi >= 4 & i.rcsi <= 18) & (i.fcs >= 21.5 & i.fcs <= 35) ~ 25,
             # 26 - 30
             i.hhs == 0 & (i.rcsi >= 4 & i.rcsi <= 18) & i.fcs  < 21.5 ~ 26,
             i.hhs == 1 & (i.rcsi >= 4 & i.rcsi <= 18) & i.fcs  < 21.5 ~ 27,
             (i.hhs >= 2 & i.hhs <= 3) & (i.rcsi >= 4 & i.rcsi <= 18) & i.fcs  < 21.5 ~ 28,
             i.hhs == 4 & (i.rcsi >= 4 & i.rcsi <= 18) & i.fcs  < 21.5 ~ 29,
             (i.hhs >= 5 & i.hhs <= 6) & (i.rcsi >= 4 & i.rcsi <= 18) & i.fcs  < 21.5 ~ 30,
             # 31 - 35
             i.hhs == 0 & i.rcsi > 18 & i.fcs > 35 ~ 31,
             i.hhs == 1 & i.rcsi > 18 & i.fcs > 35 ~ 32,
             (i.hhs >= 2 & i.hhs <= 3) & i.rcsi > 18 & i.fcs > 35 ~ 33,
             i.hhs == 4 & i.rcsi > 18 & i.fcs > 35 ~ 34,
             (i.hhs >= 5 & i.hhs <= 6) & i.rcsi > 18 & i.fcs > 35 ~ 35,
             # 36 - 40
             i.hhs == 0 & i.rcsi > 18 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 36,
             i.hhs == 1 & i.rcsi > 18 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 37,
             (i.hhs >= 2 & i.hhs <= 3) & i.rcsi > 18 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 38,
             i.hhs == 4 & i.rcsi > 18 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 39,
             (i.hhs >= 5 & i.hhs <= 6) & i.rcsi > 18 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 40,
             # 41 - 45
             i.hhs == 0 & i.rcsi > 18 & i.fcs  < 21.5 ~ 41,
             i.hhs == 1 & i.rcsi > 18 & i.fcs  < 21.5 ~ 42,
             (i.hhs >= 2 & i.hhs <= 3) & i.rcsi > 18 & i.fcs  < 21.5 ~ 43,
             i.hhs == 4 & i.rcsi > 18 & i.fcs  < 21.5 ~ 44,
             (i.hhs >= 5 & i.hhs <= 6) & i.rcsi > 18 & i.fcs  < 21.5 ~ 45),
           i.fc_matrix_cat = case_when(i.fc_matrix %in% c(1, 6) ~ "Phase 1",
                                       i.fc_matrix %in% c(2, 3, 7, 11, 12, 16, 17, 18, 21, 22, 26, 31, 32, 36) ~ "Phase 2",
                                       i.fc_matrix %in% c(4, 5, 8, 9, 13, 19, 20, 23, 24, 27, 28, 33, 34, 37, 38, 41, 42, 43) ~ "Phase 3",
                                       i.fc_matrix %in% c(10, 14, 15, 25, 29, 35, 39, 40, 44) ~ "Phase 4",
                                       i.fc_matrix %in% c(30, 45) ~ "Phase 5"),
           i.fc_matrix_fcs_hhs = case_when( 
             # 1 - 5
             i.hhs == 0 & i.fcs > 35 ~ 1,
             i.hhs == 1 & i.fcs > 35 ~ 2,
             (i.hhs >= 2 & i.hhs <= 3) & i.fcs > 35 ~ 3,
             i.hhs == 4 & i.fcs > 35 ~ 4,
             (i.hhs >= 5 & i.hhs <= 6) & i.fcs > 35 ~ 5,
             # 6 - 10
             i.hhs == 0 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 6,
             i.hhs == 1 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 7,
             (i.hhs >= 2 & i.hhs <= 3) & (i.fcs >= 21.5 & i.fcs <= 35) ~ 8,
             i.hhs == 4 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 9,
             (i.hhs >= 5 & i.hhs <= 6) & (i.fcs >= 21.5 & i.fcs <= 35) ~ 10,
             # 11 - 15
             i.hhs == 0 & i.fcs  < 21.5 ~ 11,
             i.hhs == 1 & i.fcs  < 21.5 ~ 12,
             (i.hhs >= 2 & i.hhs <= 3) & i.fcs  < 21.5 ~ 13,
             i.hhs == 4 & i.fcs  < 21.5 ~ 14,
             (i.hhs >= 5 & i.hhs <= 6) & i.fcs  < 21.5 ~ 15,
             # 16 - 20
             i.hhs == 0  & i.fcs > 35 ~ 16,
             i.hhs == 1  & i.fcs > 35 ~ 17,
             (i.hhs >= 2 & i.hhs <= 3)  & i.fcs > 35 ~ 18,
             i.hhs == 4  & i.fcs > 35 ~ 19,
             (i.hhs >= 5 & i.hhs <= 6)  & i.fcs > 35 ~ 20,
             # 21 - 25
             i.hhs == 0  & (i.fcs >= 21.5 & i.fcs <= 35) ~ 21,
             i.hhs == 1  & (i.fcs >= 21.5 & i.fcs <= 35) ~ 22,
             (i.hhs >= 2 & i.hhs <= 3)  & (i.fcs >= 21.5 & i.fcs <= 35) ~ 23,
             i.hhs == 4  & (i.fcs >= 21.5 & i.fcs <= 35) ~ 24,
             (i.hhs >= 5 & i.hhs <= 6)  & (i.fcs >= 21.5 & i.fcs <= 35) ~ 25,
             # 26 - 30
             i.hhs == 0  & i.fcs  < 21.5 ~ 26,
             i.hhs == 1  & i.fcs  < 21.5 ~ 27,
             (i.hhs >= 2 & i.hhs <= 3)  & i.fcs  < 21.5 ~ 28,
             i.hhs == 4  & i.fcs  < 21.5 ~ 29,
             (i.hhs >= 5 & i.hhs <= 6)  & i.fcs  < 21.5 ~ 30,
             # 31 - 35
             i.hhs == 0 & i.fcs > 35 ~ 31,
             i.hhs == 1 & i.fcs > 35 ~ 32,
             (i.hhs >= 2 & i.hhs <= 3) & i.fcs > 35 ~ 33,
             i.hhs == 4 & i.fcs > 35 ~ 34,
             (i.hhs >= 5 & i.hhs <= 6) & i.fcs > 35 ~ 35,
             # 36 - 40
             i.hhs == 0 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 36,
             i.hhs == 1 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 37,
             (i.hhs >= 2 & i.hhs <= 3) & (i.fcs >= 21.5 & i.fcs <= 35) ~ 38,
             i.hhs == 4 & (i.fcs >= 21.5 & i.fcs <= 35) ~ 39,
             (i.hhs >= 5 & i.hhs <= 6) & (i.fcs >= 21.5 & i.fcs <= 35) ~ 40,
             # 41 - 45
             i.hhs == 0 & i.fcs  < 21.5 ~ 41,
             i.hhs == 1 & i.fcs  < 21.5 ~ 42,
             (i.hhs >= 2 & i.hhs <= 3) & i.fcs  < 21.5 ~ 43,
             i.hhs == 4 & i.fcs  < 21.5 ~ 44,
             (i.hhs >= 5 & i.hhs <= 6) & i.fcs  < 21.5 ~ 45),
           i.fc_matrix_fcs_hhs = case_when(i.fc_matrix_fcs_hhs %in% c(1, 6) ~ "Phase 1",
                                           i.fc_matrix_fcs_hhs %in% c(2, 3, 7, 11, 12, 16, 17, 18, 21, 22, 26, 31, 32, 36) ~ "Phase 2",
                                           i.fc_matrix_fcs_hhs %in% c(4, 5, 8, 9, 13, 19, 20, 23, 24, 27, 28, 33, 34, 37, 38, 41, 42, 43) ~ "Phase 3",
                                           i.fc_matrix_fcs_hhs %in% c(10, 14, 15, 25, 29, 35, 39, 40, 44) ~ "Phase 4",
                                           i.fc_matrix_fcs_hhs %in% c(30, 45) ~ "Phase 5"),
    ) |> 
    addindicators::add_lcsi(lcsi_stress_vars = c("fsl_lcsi_en_stress1", "fsl_lcsi_en_stress2", "fsl_lcsi_en_stress3", "fsl_lcsi_en_stress4"),
                            lcsi_crisis_vars = c("fsl_lcsi_en_crisis1", "fsl_lcsi_en_crisis2", "fsl_lcsi_en_crisis3"),
                            lcsi_emergency_vars = c("fsl_lcsi_en_emergency1", "fsl_lcsi_en_emergency2", "fsl_lcsi_en_emergency3"),
                            yes_val = "yes",
                            no_val = "no_had_no_need",
                            exhausted_val = "no_exhausted",
                            not_applicable_val = "not_applicable") |> 
    # rename(i.lcsi_cat = lcsi_cat ) |> 
    select(-c(starts_with("int.")))
}

###############################################################################