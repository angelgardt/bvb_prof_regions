library(tidyverse)
theme_set(theme_bw())
theme_update(legend.position = "bottom")

## Combine SPO ----

rm(list = ls())

allfiles <- dir("SPO", recursive = TRUE, full.names = TRUE)
files <- allfiles[!str_detect(allfiles, "персонал|экстерн|Без учета")]

# readxl::read_excel(files[1],
#                    sheet = "Р2_1_1",
#                    skip = 12) %>% 
#   select(1:6) %>% 
#   rename(
#     "spec" = `1`,
#     "string" = `2`,
#     "code" = `3`,
#     "applied_budget" = `4`,
#     "applied_commerc" = `5`,
#     "accepted" = `6`
#   ) %>% 
#   filter(str_detect(code, "\\d{2}\\.\\d{2}\\.\\d{2}")) %>% 
#   mutate_all(as.character) %>%
#   mutate(file = files[1])

ds <- tibble()

for (file in files) {
  print(file)
  ## 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023
  readxl::read_excel(file,
                     sheet = "Р2_1_1",
                     skip = 12) %>%
    select(1:6) %>%
    rename(
      "spec" = `1`,
      "string" = `2`,
      "code" = `3`,
      "applied_budget" = `4`,
      "applied_commerc" = `5`,
      "accepted" = `6`
    ) %>%
    filter(str_detect(code, "\\d{2}\\.\\d{2}\\.\\d{2}")) %>%
    mutate_all(as.character) %>%
    mutate(file = file) -> current_svod
  
  current_svod %>% bind_rows(ds) -> ds
}

ds %>% 
  separate(file, 
           into = c("edulvl", "year", "type", "filename"), 
           sep = "/") %>% # View()
  separate(filename, 
           into = c("region", "del", "form"),
           sep = "_") %>% #View()
  mutate(form = form %>% 
           str_remove_all(".xls$|.xlsx$") %>% 
           tolower(),
         year = year %>% str_extract("\\d{4}$")) %>% # View() # pull(form) %>% table()
  select(!matches("^del")) -> ds_

sum(is.na(ds_))

ds_ %>% write_csv("SPO_all_combined_redo.csv")


## Combine VPO -----

rm(list = ls())

allfiles <- dir("VPO", recursive = TRUE, full.names = TRUE)
files <- allfiles[!str_detect(allfiles, "персонал|экстерн|Без учета") & str_detect(allfiles, "Своды ВПО-1 ")]

ds <- tibble()

for (file in files) {
  print(file)
  
  if (str_detect(file, "VPO_1_2023")) {
    ## 2023
    readxl::read_xls(
      file,
      sheet = "Р2_1_1",
      skip = 12
    ) %>%
      select(1:4, 8:9) %>%
      rename(
        "spec" = `1`,
        "string" = `2`,
        "code" = `3`,
        "applied_budget_total" = `4`,
        "applied_commerc" = `8`,
        "accepted" = `9`
      ) %>%
      filter(str_detect(code, "\\d{2}\\.\\d{2}\\.\\d{2}")) %>% 
      mutate(file = file) -> current_svod
  } 
  else if (str_detect(file, "VPO_1_2021|VPO_1_2022")) {
    ## 2021--2022
    readxl::read_xls(
      file,
      sheet = "Р2_1_1",
      skip = 12
    ) %>%
      select(1:4, 7:8) %>%
      rename(
        "spec" = `1`,
        "string" = `2`,
        "code" = `3`,
        "applied_budget_total" = `4`,
        "applied_commerc" = `7`,
        "accepted" = `8`
      ) %>%
      filter(str_detect(code, "\\d{2}\\.\\d{2}\\.\\d{2}")) %>% 
      mutate(file = file) -> current_svod
  } 
  else {
      ## 2016--2020
      readxl::read_xls(
        file,
        sheet = "Р2_1_1",
        skip = 11
      ) %>%
        select(1:4, 7:8) %>%
        rename(
          "spec" = `1`,
          "string" = `2`,
          "code" = `3`,
          "applied_budget_total" = `4`,
          "applied_commerc" = `7`,
          "accepted" = `8`
        ) %>%
        filter(str_detect(code, "\\d{2}\\.\\d{2}\\.\\d{2}")) %>% 
        mutate(file = file) -> current_svod
  }
  current_svod %>% 
    mutate_all(.funs = as.character) %>% 
    bind_rows(ds) -> ds
}

# ds %>% View()

ds %>% 
  separate(file, 
           into = c("edulvl", "year", "del2", "type", "filename"), 
           sep = "/") %>% # View()
  separate(filename, 
           into = c("region", "del3", "form"),
           sep = "_") %>% # View()
  mutate(form = form %>% 
           str_remove_all(".xls$") %>% 
           tolower(),
         year = year %>% str_extract("\\d{4}$")) %>% # pull(form) %>% table()
  select(!matches("^del")) -> ds_

ds_ %>% write_csv("VPO_all_combined_redo.csv")


## Preprocess & Counts -----

rm(list = ls())

## Set ROI
roi_pfo <- c("Нижегородская область", "Самарская область")
roi_usfo <- c("Тюменская область", "Омская область")
roi_ufo <- c("Ростовская область", "Волгоградская область")
roi <- c(roi_pfo, roi_ufo, roi_usfo)

## Read specs table
specs <- read_csv("specs_old+new.csv", skip = 1) %>% 
  select(field, field_name, group_code_name, edulvl, edulvl_2, code_name, code_name_full, code) %>% 
  distinct(code, .keep_all = TRUE)

spo_all_comb <- read_csv("SPO_all_combined_redo.csv")
vpo_all_comb <- read_csv("VPO_all_combined_redo.csv")

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

# vpo_all_comb %>% 
#   filter(year == 2016 &
#            region == "Нижегородская область")

spo_all_comb %>% is.na() %>% sum()

spo_all_comb %>% # colnames()
  filter(region %in% roi) %>% 
  mutate(applied = applied_commerc + applied_budget) %>% 
  select(region, year, code, spec, applied, accepted) %>% # print()
  left_join(specs, join_by(code)) -> spo_roi_joined
  # sapply(is.na) %>% apply(2, sum)
  # filter(is.na(code_name)) %>% 
  # distinct(code) %>% View()

spo_roi_joined %>% 
  summarise(applied = sum(applied, na.rm = TRUE),
            accepted = sum(accepted, na.rm = TRUE),
            .by = c(region, year, field, field_name)) %>% # print()
  arrange(region, year, field) %>% 
  pivot_longer(cols = c(applied, accepted),
               names_to = "stat") %>% # print()
  pivot_wider(names_from = year,
              values_from = value) %>% # print()
  mutate(stat = recode(stat,
                       applied = "Поданные",
                       accepted = "Принятые")) -> spo_fields_counts

vpo_all_comb %>% is.na() %>% sum()

vpo_all_comb %>% 
  filter(region %in% roi &
           string != 3 & string != 6) %>% 
  mutate(applied = applied_commerc + applied_budget_total) %>% 
  select(region, year, code, spec, applied, accepted) %>% 
  left_join(specs, join_by(code)) -> vpo_roi_joined
  # sapply(is.na) %>% apply(2, sum)
  # filter(is.na(code_name)) %>% #View()
  # distinct(code, spec) %>%
  # arrange(code) %>% View()

vpo_roi_joined %>% 
  summarise(applied = sum(applied, na.rm = TRUE),
            accepted = sum(accepted, na.rm = TRUE),
            .by = c(region, year, field, field_name)) %>% 
  arrange(region, year, field) %>% 
  pivot_longer(cols = c(applied, accepted),
               names_to = "stat") %>% 
  pivot_wider(names_from = year,
              values_from = value) %>% 
  mutate(stat = recode(stat,
                       applied = "Поданные",
                       accepted = "Принятые")) -> vpo_fields_counts


spo_roi_joined %>% 
  summarise(applied = sum(applied, na.rm = TRUE),
            accepted = sum(accepted, na.rm = TRUE),
            .by = c(region, year, field, field_name, group_code_name)) %>% 
  arrange(region, year, field, group_code_name) %>% 
  pivot_longer(cols = c(applied, accepted),
               names_to = "stat") %>% 
  pivot_wider(names_from = year,
              values_from = value) %>% 
  mutate(stat = recode(stat,
                       applied = "Поданные",
                       accepted = "Принятые")) -> spo_groups_counts

vpo_roi_joined %>% 
  summarise(applied = sum(applied, na.rm = TRUE),
            accepted = sum(accepted, na.rm = TRUE),
            .by = c(region, year, field, field_name, group_code_name)) %>% 
  arrange(region, year, field, group_code_name) %>% 
  pivot_longer(cols = c(applied, accepted),
               names_to = "stat") %>% 
  pivot_wider(names_from = year,
              values_from = value) %>% 
  mutate(stat = recode(stat,
                       applied = "Поданные",
                       accepted = "Принятые")) -> vpo_groups_counts


## SPO fields counts export -----

spo_fields_counts %>% 
  filter(region %in% roi_ufo) %>% 
  arrange(region, field) %>% 
  # write_csv("SPO_UFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "counts_SPO_UFO_fields"
  )

spo_fields_counts %>% 
  filter(region %in% roi_pfo) %>% 
  arrange(region, field) %>% 
  # write_csv("SPO_PFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "counts_SPO_PFO_fields"
  )

spo_fields_counts %>% 
  filter(region %in% roi_usfo) %>% 
  arrange(region, field) %>% 
  # write_csv("SPO_USFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "counts_SPO_USFO_fields"
  )


## VPO fields counts export -----
vpo_fields_counts %>% 
  filter(region %in% roi_ufo) %>% 
  arrange(region, field) %>% 
  # write_csv("VPO_UFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "counts_VPO_UFO_fields"
  )

vpo_fields_counts %>% 
  filter(region %in% roi_pfo) %>% 
  arrange(region, field) %>% 
  # write_csv("VPO_PFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "counts_VPO_PFO_fields"
  )

vpo_fields_counts %>% 
  filter(region %in% roi_usfo) %>% 
  arrange(region, field) %>% 
  # write_csv("VPO_USFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "counts_VPO_USFO_fields"
  )



## SPO groups counts export -----

spo_groups_counts %>% 
  filter(region %in% roi_ufo) %>% 
  arrange(region, field, group_code_name) %>% 
  # write_csv("SPO_UFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "counts_SPO_UFO_groups"
  )

spo_groups_counts %>% 
  filter(region %in% roi_pfo) %>% 
  arrange(region, field, group_code_name) %>% 
  # write_csv("SPO_PFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "counts_SPO_PFO_groups"
  )

spo_groups_counts %>% 
  filter(region %in% roi_usfo) %>% 
  arrange(region, field, group_code_name) %>% 
  # write_csv("SPO_USFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "counts_SPO_USFO_groups"
  )

## VPO groups counts export -----


vpo_groups_counts %>% 
  filter(region %in% roi_ufo) %>% 
  arrange(region, field, group_code_name) %>% 
  # write_csv("VPO_UFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "counts_VPO_UFO_groups"
  )

vpo_groups_counts %>% 
  filter(region %in% roi_pfo) %>% 
  arrange(region, field, group_code_name) %>% 
  # write_csv("VPO_PFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "counts_VPO_PFO_groups"
  )

vpo_groups_counts %>% 
  filter(region %in% roi_usfo) %>% 
  arrange(region, field, group_code_name) %>% 
  # write_csv("VPO_USFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "counts_VPO_USFO_groups"
  )

## Perc ----

spo_fields_counts %>% 
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "year") %>% 
  summarise(sum = sum(value, na.rm = TRUE),
            .by = c(region, stat, year)) %>% 
  right_join(spo_fields_counts %>% 
               pivot_longer(cols = matches("\\d{4}"),
                            names_to = "year"),
             join_by(region, stat, year)) %>% 
  mutate(perc = (value / sum * 100) %>% round(2)) %>% 
  # filter(year == 2016 & region == "Самарская область" & stat ==  "Поданные") %>% pull(perc) %>% sum()
  select(-sum, -value) %>% 
  pivot_wider(names_from = year,
              values_from = perc) %>% 
  arrange(region, field) -> spo_fields_perc


vpo_fields_counts %>% 
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "year") %>% 
  summarise(sum = sum(value, na.rm = TRUE),
            .by = c(region, stat, year)) %>% 
  right_join(vpo_fields_counts %>% 
               pivot_longer(cols = matches("\\d{4}"),
                            names_to = "year"),
             join_by(region, stat, year)) %>% 
  mutate(perc = (value / sum * 100) %>% round(2)) %>% 
  # filter(year == 2016 & region == "Самарская область" & stat ==  "Поданные") %>% pull(perc) %>% sum()
  select(-sum, -value) %>% 
  pivot_wider(names_from = year,
              values_from = perc) %>% 
  arrange(region, field) -> vpo_fields_perc

spo_groups_counts %>% 
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "year") %>% 
  summarise(sum = sum(value, na.rm = TRUE),
            .by = c(region, stat, year)) %>% 
  right_join(spo_groups_counts %>% 
               pivot_longer(cols = matches("\\d{4}"),
                            names_to = "year"),
             join_by(region, stat, year)) %>% 
  mutate(perc = (value / sum * 100) %>% round(2)) %>% 
  # filter(year == 2016 & region == "Самарская область" & stat ==  "Поданные") %>% pull(perc) %>% sum(na.rm = TRUE)
  select(-sum, -value) %>% 
  select(region, field, field_name, group_code_name, stat, year, perc) %>% 
  pivot_wider(names_from = year,
              values_from = perc) %>% 
  arrange(region, field, group_code_name) -> spo_groups_perc


vpo_groups_counts %>% 
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "year") %>% 
  summarise(sum = sum(value, na.rm = TRUE),
            .by = c(region, stat, year)) %>% 
  right_join(vpo_groups_counts %>% 
               pivot_longer(cols = matches("\\d{4}"),
                            names_to = "year"),
             join_by(region, stat, year)) %>% 
  mutate(perc = (value / sum * 100) %>% round(2)) %>% 
  # filter(year == 2016 & region == "Самарская область" & stat ==  "Поданные") %>% pull(perc) %>% sum(na.rm = TRUE)
  select(-sum, -value) %>% 
  select(region, field, field_name, group_code_name, stat, year, perc) %>% 
  pivot_wider(names_from = year,
              values_from = perc) %>% 
  arrange(region, field, group_code_name) -> vpo_groups_perc


## SPO fields perc export -----

spo_fields_perc %>% 
  filter(region %in% roi_ufo) %>% 
  arrange(region, field) %>% 
  # write_csv("SPO_UFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "perc_SPO_UFO_fields"
  )

spo_fields_perc %>% 
  filter(region %in% roi_pfo) %>% 
  arrange(region, field) %>% 
  # write_csv("SPO_PFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "perc_SPO_PFO_fields"
  )

spo_fields_perc %>% 
  filter(region %in% roi_usfo) %>% 
  arrange(region, field) %>% 
  # write_csv("SPO_USFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "perc_SPO_USFO_fields"
  )


## VPO fields perc export -----
vpo_fields_perc %>% 
  filter(region %in% roi_ufo) %>% 
  arrange(region, field) %>% 
  # write_csv("VPO_UFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "perc_VPO_UFO_fields"
  )

vpo_fields_perc %>% 
  filter(region %in% roi_pfo) %>% 
  arrange(region, field) %>% 
  # write_csv("VPO_PFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "perc_VPO_PFO_fields"
  )

vpo_fields_perc %>% 
  filter(region %in% roi_usfo) %>% 
  arrange(region, field) %>% 
  # write_csv("VPO_USFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "perc_VPO_USFO_fields"
  )



## SPO groups perc export -----

spo_groups_perc %>% 
  filter(region %in% roi_ufo) %>% 
  arrange(region, field, group_code_name) %>% 
  # write_csv("SPO_UFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "perc_SPO_UFO_groups"
  )

spo_groups_perc %>% 
  filter(region %in% roi_pfo) %>% 
  arrange(region, field, group_code_name) %>% 
  # write_csv("SPO_PFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "perc_SPO_PFO_groups"
  )

spo_groups_perc %>% 
  filter(region %in% roi_usfo) %>% 
  arrange(region, field, group_code_name) %>% 
  # write_csv("SPO_USFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "perc_SPO_USFO_groups"
  )


## VPO groups perc export -----

vpo_groups_perc %>% 
  filter(region %in% roi_ufo) %>% 
  arrange(region, field, group_code_name) %>% 
  # write_csv("VPO_UFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "perc_VPO_UFO_groups"
  )

vpo_groups_perc %>% 
  filter(region %in% roi_pfo) %>% 
  arrange(region, field, group_code_name) %>% 
  # write_csv("VPO_PFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "perc_VPO_PFO_groups"
  )

vpo_groups_perc %>% 
  filter(region %in% roi_usfo) %>% 
  arrange(region, field, group_code_name) %>% 
  # write_csv("VPO_USFO_fields_counts.csv")
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "perc_VPO_USFO_groups"
  )


## Graphs ---

spo_fields_perc %>% 
  pivot_longer(cols =  matches("\\d{4}")) -> spo_fields_perc_long
vpo_fields_perc %>% 
  pivot_longer(cols =  matches("\\d{4}")) -> vpo_fields_perc_long
spo_groups_perc %>% 
  pivot_longer(cols =  matches("\\d{4}")) %>% 
  mutate(gr_code = str_extract(group_code_name, "^\\d{1}\\.\\d{2}") %>% str_remove("^\\d{1}\\.")) -> spo_groups_perc_long
vpo_groups_perc %>% 
  pivot_longer(cols =  matches("\\d{4}")) %>% 
  mutate(gr_code = str_extract(group_code_name, "^\\d{1}\\.\\d{2}") %>% str_remove("^\\d{1}\\.")) -> vpo_groups_perc_long

spo_fields_counts %>% 
  pivot_longer(cols =  matches("\\d{4}")) -> spo_fields_counts_long
vpo_fields_counts %>% 
  pivot_longer(cols =  matches("\\d{4}")) -> vpo_fields_counts_long
spo_groups_counts %>% 
  pivot_longer(cols =  matches("\\d{4}")) %>% 
  mutate(gr_code = str_extract(group_code_name, "^\\d{1}\\.\\d{2}") %>% str_remove("^\\d{1}\\.")) -> spo_groups_counts_long
vpo_groups_counts %>% 
  pivot_longer(cols =  matches("\\d{4}")) %>% 
  mutate(gr_code = str_extract(group_code_name, "^\\d{1}\\.\\d{2}") %>% str_remove("^\\d{1}\\.")) -> vpo_groups_counts_long



spo_fields_perc_long %>% 
  filter(region %in% roi_ufo) %>% 
  ggplot(aes(name, value, 
             shape = stat, 
             linetype = stat,
             group = stat)) +
  geom_line() +
  geom_point() +
  # geom_text(aes(label = value), 
  #           size = 3, position = position_nudge(x = .3)) +
  facet_grid(field ~ region,
             scales = "free_y") +
  # scale_linetype_manual(values = c(perc_applied = "solid",
  #                                  perc_accepted = "dashed")) +
  # scale_shape_manual(values = c(perc_applied = 16,
  #                               perc_accepted = 17)) +
  labs(x = "Год", y = "Процент",
       shape = "", linetype = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "CПО")

spo_fields_perc_long %>% 
  filter(region %in% roi_pfo) %>% 
  ggplot(aes(name, value, 
             shape = stat, 
             linetype = stat,
             group = stat)) +
  geom_line() +
  geom_point() +
  # geom_text(aes(label = value), 
  #           size = 3, position = position_nudge(x = .3)) +
  facet_grid(field ~ region,
             scales = "free_y") +
  # scale_linetype_manual(values = c(perc_applied = "solid",
  #                                  perc_accepted = "dashed")) +
  # scale_shape_manual(values = c(perc_applied = 16,
  #                               perc_accepted = 17)) +
  labs(x = "Год", y = "Процент",
       shape = "", linetype = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "CПО")

spo_fields_perc_long %>% 
  filter(region %in% roi_usfo) %>% 
  ggplot(aes(name, value, 
             shape = stat, 
             linetype = stat,
             group = stat)) +
  geom_line() +
  geom_point() +
  # geom_text(aes(label = value), 
  #           size = 3, position = position_nudge(x = .3)) +
  facet_grid(field ~ region,
             scales = "free_y") +
  # scale_linetype_manual(values = c(perc_applied = "solid",
  #                                  perc_accepted = "dashed")) +
  # scale_shape_manual(values = c(perc_applied = 16,
  #                               perc_accepted = 17)) +
  labs(x = "Год", y = "Процент",
       shape = "", linetype = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "CПО")

# spo_fields_counts_long %>% 
#   filter(region %in% roi_ufo) %>% 
#   ggplot(aes(name, value, 
#              shape = stat, 
#              linetype = stat,
#              group = stat)) +
#   geom_line() +
#   geom_point() +
#   facet_grid(field ~ region,
#              scales = "free_y") +
#   # scale_linetype_manual(values = c(perc_applied = "solid",
#   #                                  perc_accepted = "dashed")) +
#   # scale_shape_manual(values = c(perc_applied = 16,
#   #                               perc_accepted = 17)) +
#   labs(x = "Год", y = "Количество",
#        shape = "", linetype = "",
#        title = "Динамика поданных/принятых заявлений",
#        subtitle = "CПО")



vpo_fields_perc_long %>% 
  filter(region %in% roi_ufo) %>% 
  ggplot(aes(name, value, 
             shape = stat, 
             linetype = stat,
             group = stat)) +
  geom_line() +
  geom_point() +
  facet_grid(field ~ region,
             scales = "free_y") +
  # scale_linetype_manual(values = c(perc_applied = "solid",
  #                                  perc_accepted = "dashed")) +
  # scale_shape_manual(values = c(perc_applied = 16,
  #                               perc_accepted = 17)) +
  labs(x = "Год", y = "Процент",
       shape = "", linetype = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "BПО")

vpo_fields_perc_long %>% 
  filter(region %in% roi_pfo) %>% 
  ggplot(aes(name, value, 
             shape = stat, 
             linetype = stat,
             group = stat)) +
  geom_line() +
  geom_point() +
  facet_grid(field ~ region,
             scales = "free_y") +
  # scale_linetype_manual(values = c(perc_applied = "solid",
  #                                  perc_accepted = "dashed")) +
  # scale_shape_manual(values = c(perc_applied = 16,
  #                               perc_accepted = 17)) +
  labs(x = "Год", y = "Процент",
       shape = "", linetype = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "BПО")

vpo_fields_perc_long %>% 
  filter(region %in% roi_usfo) %>% 
  ggplot(aes(name, value, 
             shape = stat, 
             linetype = stat,
             group = stat)) +
  geom_line() +
  geom_point() +
  facet_grid(field ~ region,
             scales = "free_y") +
  # scale_linetype_manual(values = c(perc_applied = "solid",
  #                                  perc_accepted = "dashed")) +
  # scale_shape_manual(values = c(perc_applied = 16,
  #                               perc_accepted = 17)) +
  labs(x = "Год", y = "Процент",
       shape = "", linetype = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "BПО")


# vpo_fields_counts_long %>% 
#   filter(region %in% roi_ufo) %>% 
#   ggplot(aes(name, value, 
#              shape = stat, 
#              linetype = stat,
#              group = stat)) +
#   geom_line() +
#   geom_point() +
#   facet_grid(field ~ region,
#              scales = "free_y") +
#   # scale_linetype_manual(values = c(perc_applied = "solid",
#   #                                  perc_accepted = "dashed")) +
#   # scale_shape_manual(values = c(perc_applied = 16,
#   #                               perc_accepted = 17)) +
#   labs(x = "Год", y = "Количество",
#        shape = "", linetype = "",
#        title = "Динамика поданных/принятых заявлений",
#        subtitle = "BПО")

fields <- specs %>% 
  select(field, field_name) %>% 
  distinct()

roi_list <- list(ufo = roi_ufo,
                 pfo = roi_pfo,
                 usfo = roi_usfo)


spo_groups_perc_long %>% 
  filter(region %in% roi_list$ufo &
           field == 1) %>% 
  ggplot(aes(name, value, 
             shape = stat, 
             linetype = stat,
             group = stat)) +
  geom_line() +
  geom_point() +
  # geom_text(aes(label = value), 
  #           size = 3, position = position_nudge(x = .3)) +
  facet_grid(gr_code ~ region,
             scales = "free_y") +
  # scale_linetype_manual(values = c(perc_applied = "solid",
  #                                  perc_accepted = "dashed")) +
  # scale_shape_manual(values = c(perc_applied = 16,
  #                               perc_accepted = 17)) +
  labs(x = "Год", y = "Процент",
       shape = "", linetype = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = paste0("CПО, ", fields$field_name[f]))


for (f in 1:8) {
  for (r in roi_list) {
    print(paste0("FIELD ", f))
    print(paste0("ROI ", r))
    
    print("SPO")
    
    spo_groups_perc_long %>% 
    filter(region %in% r &
             field == f) -> d
    
    if (nrow(d) == 0) next
    
    d %>% 
      ggplot(aes(name, value, 
               shape = stat, 
               linetype = stat,
               group = stat)) +
    geom_line() +
    geom_point() +
    # geom_text(aes(label = value), 
    #           size = 3, position = position_nudge(x = .3)) +
    facet_grid(gr_code ~ region,
               scales = "free_y") +
    # scale_linetype_manual(values = c(perc_applied = "solid",
    #                                  perc_accepted = "dashed")) +
    # scale_shape_manual(values = c(perc_applied = 16,
    #                               perc_accepted = 17)) +
    labs(x = "Год", y = "Процент",
         shape = "", linetype = "",
         title = "Динамика поданных/принятых заявлений",
         subtitle = paste0("CПО, ", fields$field_name[f])) -> current_graph
    print(current_graph)
    
    print("VPO")
    
    vpo_groups_perc_long %>% 
      filter(region %in% r &
               field == f) -> d
    
    if (nrow(d) == 0) next
    
    d %>% 
      ggplot(aes(name, value, 
                 shape = stat, 
                 linetype = stat,
                 group = stat)) +
      geom_line() +
      geom_point() +
      # geom_text(aes(label = value), 
      #           size = 3, position = position_nudge(x = .3)) +
      facet_grid(gr_code ~ region,
                 scales = "free_y") +
      # scale_linetype_manual(values = c(perc_applied = "solid",
      #                                  perc_accepted = "dashed")) +
      # scale_shape_manual(values = c(perc_applied = 16,
      #                               perc_accepted = 17)) +
      labs(x = "Год", y = "Процент",
           shape = "", linetype = "",
           title = "Динамика поданных/принятых заявлений",
           subtitle = paste0("ВПО, ", fields$field_name[f])) -> current_graph
    print(current_graph)
    
  }
}


# spo_groups_perc_long %>%
#   filter(region %in% roi_usfo &
#            field == 1)  %>%
#   ggplot(aes(
#     name,
#     value,
#     shape = stat,
#     linetype = stat,
#     group = stat
#   )) +
#   geom_line() +
#   geom_point() +
#   # geom_text(aes(label = value),
#   #           size = 3, position = position_nudge(x = .3)) +
#   facet_grid(gr_code ~ region,
#              scales = "free_y") +
#   # scale_linetype_manual(values = c(perc_applied = "solid",
#   #                                  perc_accepted = "dashed")) +
#   # scale_shape_manual(values = c(perc_applied = 16,
#   #                               perc_accepted = 17)) +
#   labs(
#     x = "Год",
#     y = "Процент",
#     shape = "",
#     linetype = "",
#     title = "Динамика поданных/принятых заявлений",
#     subtitle = "CПО, Математические и естественные науки"
#   )

vpo_groups_perc_long %>%
  filter(region %in% roi_usfo &
           field == 1) %>%
  ggplot(aes(
    name,
    value,
    shape = stat,
    linetype = stat,
    group = stat
  )) +
  geom_line() +
  geom_point() +
  # geom_text(aes(label = value),
  #           size = 3, position = position_nudge(x = .3)) +
  facet_grid(gr_code ~ region,
             scales = "free_y") +
  # scale_linetype_manual(values = c(perc_applied = "solid",
  #                                  perc_accepted = "dashed")) +
  # scale_shape_manual(values = c(perc_applied = 16,
  #                               perc_accepted = 17)) +
  labs(
    x = "Год",
    y = "Процент",
    shape = "",
    linetype = "",
    title = "Динамика поданных/принятых заявлений",
    subtitle = paste0("ВПО, ", fields$field_name[1])
  )

save.image("redo.RData")
