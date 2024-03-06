library(tidyverse)
theme_set(theme_bw())

##### Cobmine all data SPO -----

allfiles <- dir("SPO", recursive = TRUE, full.names = TRUE)
files <- allfiles[!str_detect(allfiles, "персонал|экстерн|Без учета")]

ds <- tibble()

for (file in files) {
  print(file)
  ## 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023
  readxl::read_excel(file,
                     sheet = "Р2_1_1",
                     skip = 12) %>%
    select(1:9, 11, 13, 14:15) %>%
    rename(
      "spec" = `1`,
      "string" = `2`,
      "code" = `3`,
      "applied_budget" = `4`,
      "applied_commerc" = `5`,
      "accepted_total" = `6`,
      "accepted_basic" = `7`,
      "accepted_indepth" = `8`,
      "accepted_budget_federal" = `9`,
      "accepted_budget_regional" = `11`,
      "accepted_budget_local" = `13`,
      "accepted_commerc" = `14`,
      "accepted_total_women" = `15`
    ) %>%
    filter(str_detect(code, "\\d{2}\\.\\d{2}\\.\\d{2}")) %>%
    mutate_all(as.character) %>%
    mutate(file = file) -> current_svod
  
  current_svod %>% bind_rows(ds) -> ds
}

ds %>% 
  separate(file, 
           into = c("edulvl", "year", "type", "filename"), 
           sep = "/") %>% 
  separate(filename, 
           into = c("region", "del", "form"),
           sep = "_") %>% #View()
  mutate(form = form %>% 
           str_remove_all(".xls$|.xlsx$") %>% 
           tolower(),
         year = year %>% str_extract("\\d{4}$")) %>% # pull(form) %>% table()
  select(!matches("^del")) -> ds_

ds_ %>% View()

ds_ %>% write_csv("SPO_all_combined.csv")

rm(list = ls())

##### Preprocess -----

rm(list = ls())

SPO <- read_csv("SPO_all_combined.csv")
spo_specs <- read_csv2("specs_old+new.csv", skip = 1) %>% 
  select(field, field_name, group_code_name, edulvl, edulvl_2, code_name, code_name_full, code) %>% 
  distinct(code, .keep_all = TRUE)

colnames(spo_specs)
nrow(spo_specs)
spo_specs$code %>% unique() %>% length()

SPO %>% nrow()

SPO %>% 
  left_join(spo_specs,
            by = join_by(code)) -> SPO_specgroups

# SPO_specgroups %>% 
  # pull(group_code_name) %>% is.na() %>% sum()
  # filter(is.na(group_code_name)) %>% 
  # distinct(spec.x, code) %>% View()

colnames(SPO_specgroups)
nrow(SPO_specgroups)
distinct(SPO_specgroups) %>% nrow()

SPO_specgroups %>% 
  mutate(accepted_budget_total = accepted_budget_federal +
           accepted_budget_regional + accepted_budget_local,
         konkurs_budget = (applied_budget / accepted_budget_total) %>% round(2),
         konkurs_commerc = (applied_commerc / accepted_commerc) %>% round(2)) %>% 
  mutate(konkurs_budget = ifelse(is.finite(konkurs_budget), konkurs_budget, NaN),
         konkurs_commerc = ifelse(is.finite(konkurs_commerc), konkurs_commerc, NaN)) %>% 
  select(
    region, 
    field, 
         field_name, 
         group_code_name, 
         string,
         code, 
         spec, 
         edulvl = edulvl_2, # уровень: среднее звено и профессии
         year, 
         form, # формы: очное, очно-заочное
         type, # ВУЗЫ гос. и не гос. вузы
         applied_budget,
         applied_commerc,
         accepted_budget_total,
         accepted_commerc,
         konkurs_budget,
         konkurs_commerc) %>% 
  arrange(region, year, field, code) -> spo_all_regions_stats

nrow(spo_all_regions_stats)

# spo_all_regions_stats %>% distinct(.keep_all = TRUE)

spo_roi <- c("Ростовская область", "Волгоградская область")  

spo_all_regions_stats %>% 
  filter(region %in% spo_roi) -> spo_roi_stats

# spo_roi_stats %>% #View()
#   googlesheets4::write_sheet(
#     ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
#     sheet = "SPO_roi_specs"
#   )

# spo_all_regions_stats %>% #View()
#   googlesheets4::write_sheet(
#     ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
#     sheet = "SPO_allreg_specs"
#   )

colnames(spo_roi_stats)

spo_roi_stats %>% 
  group_by(field,
           field_name,
           region, 
           year,
           group_code_name,
           edulvl,
           type,
           form) %>% 
  summarise(
    applied_budget_total_sum = sum(applied_budget, 
                                   na.rm = TRUE),
    applied_budget_total_mean = mean(applied_budget, 
                                     na.rm = TRUE) %>% round(2),
    accepted_budget_total_sum = sum(accepted_budget_total, 
                                    na.rm = TRUE),
    accepted_budget_total_mean = mean(accepted_budget_total, 
                                      na.rm = TRUE) %>% round(2),
    applied_commerc_sum = sum(applied_commerc, 
                              na.rm = TRUE),
    applied_commerc_mean = mean(applied_commerc, 
                                na.rm = TRUE) %>% round(2),
    accepted_commerc_sum = sum(accepted_commerc,
                               na.rm = TRUE),
    accepted_commerc_mean = mean(accepted_commerc, 
                                 na.rm = TRUE) %>% round(2),
    konkurs_budget_mean = weighted.mean(konkurs_budget,
                                        w = applied_budget,
                                        na.rm = TRUE) %>% round(2),
    konkurs_commerce_mean = weighted.mean(konkurs_commerc,
                                          w = applied_commerc,
                                          na.rm = TRUE) %>% round(2)
  ) %>% 
  arrange(region, year, field, group_code_name) -> spo_roi_avgs


spo_all_regions_stats %>% 
  group_by(field,
           field_name,
           region, 
           year,
           group_code_name,
           edulvl,
           type,
           form) %>% 
  summarise(
    applied_budget_total_sum = sum(applied_budget, 
                                   na.rm = TRUE),
    applied_budget_total_mean = mean(applied_budget, 
                                     na.rm = TRUE) %>% round(2),
    accepted_budget_total_sum = sum(accepted_budget_total, 
                                    na.rm = TRUE),
    accepted_budget_total_mean = mean(accepted_budget_total, 
                                      na.rm = TRUE) %>% round(2),
    applied_commerc_sum = sum(applied_commerc, 
                              na.rm = TRUE),
    applied_commerc_mean = mean(applied_commerc, 
                                na.rm = TRUE) %>% round(2),
    accepted_commerc_sum = sum(accepted_commerc,
                               na.rm = TRUE),
    accepted_commerc_mean = mean(accepted_commerc, 
                                 na.rm = TRUE) %>% round(2),
    konkurs_budget_mean = weighted.mean(konkurs_budget,
                                        w = applied_budget,
                                        na.rm = TRUE) %>% round(2),
    konkurs_commerce_mean = weighted.mean(konkurs_commerc,
                                          w = applied_commerc,
                                          na.rm = TRUE) %>% round(2)
  ) %>% 
  arrange(region, year, field, group_code_name) -> spo_allreg_avgs

spo_all_regions_stats %>% 
  group_by(field,
           field_name,
           region, 
           year) %>% 
  summarise(
    applied_budget_total_sum = sum(applied_budget, 
                                   na.rm = TRUE),
    accepted_budget_total_sum = sum(accepted_budget_total, 
                                    na.rm = TRUE),
    applied_commerc_sum = sum(applied_commerc, 
                              na.rm = TRUE),
    accepted_commerc_sum = sum(accepted_commerc,
                               na.rm = TRUE),
    konkurs_budget_mean = weighted.mean(konkurs_budget,
                                        w = applied_budget,
                                        na.rm = TRUE) %>% round(2),
    konkurs_commerce_mean = weighted.mean(konkurs_commerc,
                                          w = applied_commerc,
                                          na.rm = TRUE) %>% round(2)
  ) %>% 
  arrange(region, year, field) -> spo_allreg_fields

save.image("spo_export.RData")

# spo_roi_avgs %>% #View()
#   googlesheets4::write_sheet(
#     ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
#     sheet = "SPO_roi_groups"
#   )

# allreg_avgs %>% #View()
#   googlesheets4::write_sheet(
#     ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
#     sheet = "SPO_allreg_groups"
#   )
