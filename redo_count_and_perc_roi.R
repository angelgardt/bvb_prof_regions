library(tidyverse)

rm(list = ls())

roi_pfo <- c("Нижегородская область", "Самарская область")
roi_usfo <- c("Тюменская область", "Омская область")
roi_ufo <- c("Ростовская область", "Волгоградская область")
roi <- c(roi_pfo, roi_ufo, roi_usfo)

specs <- read_csv2("specs_old+new.csv", skip = 1) %>% 
  select(field, field_name, group_code_name, edulvl, edulvl_2, code_name, code_name_full, code) %>% 
  distinct(code, .keep_all = TRUE)

spo_all_comb <- read_csv("SPO_all_combined.csv")
vpo_all_comb <- read_csv("VPO_all_combined.csv")

# spo_all_comb %>% 
#   filter(region == "Тюменская область" &
#            year == 2018 &
#            str_detect(code, "^35"))
# 
# spo_allreg_stats %>% 
#   filter(region == "Тюменская область" &
#            year == 2018 &
#            str_detect(code, "^35")) %>% View
# 
# spo_allreg_avgs %>% 
#   filter(region == "Тюменская область" &
#          year == 2018 &
#          str_detect(group_code_name, "^4\\.35")) %>% View

vpo_all_comb %>% 
  filter(year == 2016 &
           region == "Нижегородская область")

spo_all_comb %>% 
  filter(region %in% roi) %>% 
  mutate(applied_total = applied_commerc + applied_budget) %>% 
  select(region, year, code, spec, applied_total, accepted_total) %>% 
  left_join(specs, join_by(code)) %>% 
  summarise(applied = sum(applied_total),
            accepted = sum(accepted_total),
            .by = c(region, year, field, field_name)) %>% 
  arrange(region, year, field) %>% 
  pivot_longer(cols = c(applied, accepted),
               names_to = "stat") %>% 
  pivot_wider(names_from = year,
              values_from = value) %>% 
  mutate(stat = recode(stat,
                       applied = "Поданные",
                       accepted = "Принятые")) -> spo_fields_counts

vpo_all_comb %>% 
  filter(region %in% roi) %>% 
  mutate(applied_total = applied_commerc + applied_budget) %>% 
  select(region, year, code, spec, applied_total, accepted_total) %>% 
  left_join(specs, join_by(code)) %>% 
  summarise(applied = sum(applied_total),
            accepted = sum(accepted_total),
            .by = c(region, year, field, field_name)) %>% 
  arrange(region, year, field) %>% 
  pivot_longer(cols = c(applied, accepted),
               names_to = "stat") %>% 
  pivot_wider(names_from = year,
              values_from = value) %>% 
  mutate(stat = recode(stat,
                       applied = "Поданные",
                       accepted = "Принятые")) -> vpo_fields_counts



spo_all_comb %>% 
  filter(region %in% roi) %>% 
  mutate(applied_total = applied_commerc + applied_budget) %>% 
  select(region, year, code, spec, applied_total, accepted_total) %>% 
  left_join(specs, join_by(code)) %>% 
  summarise(applied = sum(applied_total),
            accepted = sum(accepted_total),
            .by = c(region, year, field, field_name, group_code_name)) %>% 
  arrange(region, year, field, group_code_name) %>% 
  pivot_longer(cols = c(applied, accepted),
               names_to = "stat") %>% 
  pivot_wider(names_from = year,
              values_from = value) %>% 
  mutate(stat = recode(stat,
                       applied = "Поданные",
                       accepted = "Принятые")) -> spo_groups_counts

vpo_all_comb %>% 
  filter(region %in% roi) %>% 
  mutate(applied_total = applied_commerc + applied_budget) %>% 
  select(region, year, code, spec, applied_total, accepted_total) %>% 
  left_join(specs, join_by(code)) %>% 
  summarise(applied = sum(applied_total),
            accepted = sum(accepted_total),
            .by = c(region, year, field, field_name, group_code_name)) %>% 
  arrange(region, year, field, group_code_name) %>% 
  pivot_longer(cols = c(applied, accepted),
               names_to = "stat") %>% 
  pivot_wider(names_from = year,
              values_from = value) %>% 
  mutate(stat = recode(stat,
                       applied = "Поданные",
                       accepted = "Принятые")) -> vpo_groups_counts

spo_fields_counts %>% 
  filter(region %in% roi_ufo) %>% 
  # write_csv("SPO_UFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "counts_SPO_UFO_fields"
  )

spo_fields_counts %>% 
  filter(region %in% roi_pfo) %>% 
  # write_csv("SPO_PFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "counts_SPO_PFO_fields"
  )

spo_fields_counts %>% 
  filter(region %in% roi_usfo) %>% 
  # write_csv("SPO_USFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "counts_SPO_USFO_fields"
  )



vpo_fields_counts %>% 
  filter(region %in% roi_ufo) %>% 
  # write_csv("VPO_UFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "counts_VPO_UFO_fields"
  )

vpo_fields_counts %>% 
  filter(region %in% roi_pfo) %>% 
  # write_csv("VPO_PFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "counts_VPO_PFO_fields"
  )

vpo_fields_counts %>% 
  filter(region %in% roi_usfo) %>% 
  # write_csv("VPO_USFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "counts_VPO_USFO_fields"
  )





