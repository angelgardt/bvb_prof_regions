library(tidyverse)
theme_set(theme_bw())


##### Cobmine all data VPO -----

rm(list = ls())
## Общие колонки
# 1 Наименование направления подготовки (специальности) по перечням, утв. приказом Минобрнауки России от 12.09.2013 г. № 1061
# 3 Код направ-ления подготов-ки (специаль-ности)

## Принято человек
# 33 за счет бюджетных ассигнований кроме квоты приема на целевое обучение, квоты приема лиц, имеющих особое право и отдельной квоты
# 34 на места в рамках квоты приема на целевое обучение
# 35 на места в пределах квоты приема лиц, имеющих особое право
# 36 на места  пределах отдельной квоты приема
# 37 по договорам об оказании платных образовательных услуг

## Среднее количество баллов ЕГЭ
# 38 за счет бюджетных ассигнований кроме квоты приема на целевое обучение, квоты приема лиц, имеющих особое право и отдельной квоты
# 39 на места в рамках квоты приема на целевое обучение
# 40 на места в пределах квоты приема лиц, имеющих особое право
# 41 на места  пределах отдельной квоты приема
# 42 по договорам об оказании платных образовательных услуг


allfiles <- dir("VPO", recursive = TRUE, full.names = TRUE)
files <- allfiles[!str_detect(allfiles, "персонал|экстерн") & str_detect(allfiles, "Своды ВПО-1 ")]

ds <- tibble()

for (file in files) {
  print(file)
  
  if (str_detect(file, "VPO_1_2016")) {
    ## 2016
    readxl::read_xls(
      file,
      sheet = "Р2_1_1",
      skip = 11
    ) %>%
      select(1:4, 7:9, 11:12, 15:16) %>%
      rename(
        "spec" = `1`,
        "string" = `2`,
        "code" = `3`,
        "applied_budget_total" = `4`,
        "applied_commerc" = `7`,
        "accepted_total" = `8`,
        "accepted_budget_federal" = `9`,
        "accepted_budget_regional" = `11`,
        "accepted_budget_local" = `12`,
        "accepted_commerc" = `15`,
        "accepted_total_women" = `16`
      ) %>%
      filter(str_detect(code, "\\d{2}\\.\\d{2}\\.\\d{2}")) %>% 
      mutate(file = file) -> current_svod
  } 
  else if (str_detect(file, "VPO_1_2017|VPO_1_2018|VPO_1_2019|VPO_1_2020")) {
    ## 2017, 2018, 2019, 2020
    readxl::read_xls(
      file,
      sheet = "Р2_1_1",
      skip = 11
    ) %>%
      select(1:4, 7:8, 10, 12:13, 16:17) %>%
      rename(
        "spec" = `1`,
        "string" = `2`,
        "code" = `3`,
        "applied_budget_total" = `4`,
        "applied_commerc" = `7`,
        "accepted_total" = `8`,
        "accepted_budget_federal" = `10`,
        "accepted_budget_regional" = `12`,
        "accepted_budget_local" = `13`,
        "accepted_commerc" = `16`,
        "accepted_total_women" = `17`
      ) %>%
      filter(str_detect(code, "\\d{2}\\.\\d{2}\\.\\d{2}")) %>% 
      mutate(file = file) -> current_svod
  } 
  else if (str_detect(file, "VPO_1_2021|VPO_1_2022")) {
    ## 2021, 2022
    readxl::read_xls(
      file,
      sheet = "Р2_1_1",
      skip = 12
    ) %>%
      mutate_all(.funs = as.character) %>% 
      inner_join(
        readxl::read_xls(
          file,
          sheet = "Р2_1_1 (2)",
          skip = 11
        ) %>% mutate_all(.funs = as.character),
        join_by(`1`, `2`, `3`)
      ) %>%
      inner_join(
        readxl::read_xls(
          file,
          sheet = "Р2_1_1 (3)",
          skip = 11
        ) %>% mutate_all(.funs = as.character),
        join_by(`1`, `2`, `3`)
      ) %>%
      select(1:8, 10, 12:13, 16:17, 26:37) %>%
      rename(
        "spec" = `1`,
        "string" = `2`,
        "code" = `3`,
        "applied_budget_total" = `4`,
        "applied_budget_sponsored" = `5`,
        "applied_budget_specright" = `6`,
        # "applied_budget_quota" = `7`,
        "applied_commerc" = `7`,
        "accepted_total" = `8`,
        "accepted_budget_federal" = `10`,
        "accepted_budget_regional" = `12`,
        "accepted_budget_local" = `13`,
        "accepted_commerc" = `16`,
        "accepted_total_women" = `17`,
        "ege_min_budget_only" = `26`,
        "ege_min_budget_sponsored" = `27`,
        "ege_min_budget_specright" = `28`,
        # "ege_min_budget_quota" = `31`,
        "ege_min_commerc" = `29`,
        "ege_accepted_budget_only" = `30`,
        "ege_accepted_budget_sponsored" = `31`,
        "ege_accepted_budget_specright" = `32`,
        # "ege_accepted_budget_quota" = `36`,
        "ege_accepted_commerc" = `33`,
        "ege_mean_budget_only" = `34`,
        "ege_mean_budget_sponsored" = `35`,
        "ege_mean_budget_specright" = `36`,
        # "ege_mean_budget_quota" = `41`,
        "ege_mean_commerc" = `37`
      ) %>%
      filter(str_detect(code, "\\d{2}\\.\\d{2}\\.\\d{2}")) %>% 
      mutate(file = file) -> current_svod
  } 
  else if (str_detect(file, "VPO_1_2023")) {
    ## 2023
    readxl::read_xls(file,
                     sheet = "Р2_1_1",
                     skip = 12) %>%
      mutate_all(.funs = as.character) %>% 
      inner_join(
        readxl::read_xls(
          file,
          sheet = "Р2_1_1 (2)",
          skip = 11
        ) %>% mutate_all(.funs = as.character),
        join_by(`1`, `2`, `3`)
      ) %>%
      inner_join(
        readxl::read_xls(
          file,
          sheet = "Р2_1_1 (3)",
          skip = 11
        ) %>% mutate_all(.funs = as.character),
        join_by(`1`, `2`, `3`)
      ) %>%
      select(1:9, 11, 13:14, 18:19, 28:42) %>%
      rename(
        "spec" = `1`,
        "string" = `2`,
        "code" = `3`,
        "applied_budget_total" = `4`,
        "applied_budget_sponsored" = `5`,
        "applied_budget_specright" = `6`,
        "applied_budget_quota" = `7`,
        "applied_commerc" = `8`,
        "accepted_total" = `9`,
        "accepted_budget_federal" = `11`,
        "accepted_budget_regional" = `13`,
        "accepted_budget_local" = `14`,
        "accepted_commerc" = `18`,
        "accepted_total_women" = `19`,
        "ege_min_budget_only" = `28`,
        "ege_min_budget_sponsored" = `29`,
        "ege_min_budget_specright" = `30`,
        "ege_min_budget_quota" = `31`,
        "ege_min_commerc" = `32`,
        "ege_accepted_budget_only" = `33`,
        "ege_accepted_budget_sponsored" = `34`,
        "ege_accepted_budget_specright" = `35`,
        "ege_accepted_budget_quota" = `36`,
        "ege_accepted_commerc" = `37`,
        "ege_mean_budget_only" = `38`,
        "ege_mean_budget_sponsored" = `39`,
        "ege_mean_budget_specright" = `40`,
        "ege_mean_budget_quota" = `41`,
        "ege_mean_commerc" = `42`
      ) %>%
      filter(str_detect(code, "\\d{2}\\.\\d{2}\\.\\d{2}")) %>%
      mutate(file = file) -> current_svod
  }
  current_svod %>% 
    mutate_all(.funs = as.character) %>% 
    bind_rows(ds) -> ds
}

ds %>% 
  filter(!str_detect(file, "Без учета")) %>% 
  separate(file, 
           into = c("edulvl", "year", "del2", "type", "filename"), 
           sep = "/") %>%
  separate(filename, 
           into = c("region", "del3", "form"),
           sep = "_") %>% 
  mutate(form = form %>% 
           str_remove_all(".xls$") %>% 
           tolower(),
         year = year %>% str_extract("\\d{4}$")) %>% # pull(form) %>% table()
  select(!matches("^del")) -> ds_
  
ds_ %>% write_csv("VPO_all_combined.csv")

rm(list = ls())

##### Preprocess -----

rm(list=ls())

VPO <- read_csv("VPO_all_combined.csv")

# VPO %>% 
#   filter(code == "08.03.01" & year == 2016)

vpo_specs <- read_csv2("specs_old+new.csv", skip = 1) %>% 
# vpo_specs <- read_csv2("Укрупненные специальности.csv") %>% 
  # mutate(field = group_code_name %>% str_extract("^\\d{1}")) %>% 
  select(field, field_name, group_code_name, edulvl, edulvl_2, code_name, code) %>% 
  distinct(code, .keep_all = TRUE)

nrow(vpo_specs)
vpo_specs$code %>% unique() %>% length()

vpo_specs %>% 
  distinct(field, field_name)

colnames(vpo_specs)

VPO %>% 
  filter(!str_detect(code, "\\d{2}\\.04\\.\\d{2}")) %>% 
  left_join(vpo_specs,
            by = join_by(code)) -> VPO_specgroups 
  # pull(group_code_name) %>% is.na() %>% sum()
  # filter(is.na(group_code_name))

VPO_specgroups %>% 
  mutate(accepted_budget_total = accepted_budget_federal +
           accepted_budget_regional + accepted_budget_local,
         konkurs_budget = (applied_budget_total / accepted_budget_total) %>% round(2),
         konkurs_commerc = (applied_commerc / accepted_commerc) %>% round(2)) %>% 
  mutate(konkurs_budget = ifelse(is.finite(konkurs_budget), konkurs_budget, NaN),
         konkurs_commerc = ifelse(is.finite(konkurs_commerc), konkurs_commerc, NaN)) %>% 
  select(field, 
         field_name, 
         group_code_name, 
         code, 
         spec, 
         edulvl = edulvl_2, # уровень: бакалавриат и специалитет 
         region, 
         year, 
         form, # формы: очное, очно-заочное
         type, # ВУЗЫ гос. и не гос. вузы
         applied_budget_total,
         applied_commerc,
         accepted_budget_total,
         accepted_commerc,
         konkurs_budget,
         konkurs_commerc,
         ege_accepted_budget_only,
         ege_min_budget_only,
         ege_mean_budget_only,
         ege_accepted_commerc,
         ege_min_commerc,
         ege_mean_commerc) %>% 
  arrange(region, year, field, code) -> vpo_all_regions_stats

vpo_roi <- c("Ростовская область", "Волгоградская область")

vpo_all_regions_stats %>% 
  filter(region %in% vpo_roi) -> vpo_roi_stats

vpo_roi_stats %>% #View()
  googlesheets4::write_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
  sheet = "VPO_roi_specs"
)

vpo_all_regions_stats %>% #View()
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "VPO_allreg_specs"
  )

# vpo_roi_stats %>% 
#   filter(field == 2 & year == 2016 & region == "Волгоградская область") %>% 
#   filter(group_code_name %>% str_detect("2.08.00.00")) %>%
#   group_by(field,
#            field_name,
#            region, 
#            type,
#            year,
#            group_code_name,
#            edulvl,
#            form) %>% 
#   summarise(
#     applied_budget_total_sum = sum(applied_budget_total, 
#                                    na.rm = TRUE),
#     applied_budget_total_mean = mean(applied_budget_total, 
#                                      na.rm = TRUE) %>% round(2),
#     accepted_budget_total_sum = sum(accepted_budget_total, 
#                                     na.rm = TRUE),
#     accepted_budget_total_mean = mean(accepted_budget_total, 
#                                       na.rm = TRUE) %>% round(2),
#     applied_commerc_sum = sum(applied_commerc, 
#                               na.rm = TRUE),
#     applied_commerc_mean = mean(applied_commerc, 
#                                 na.rm = TRUE) %>% round(2),
#     accepted_commerc_sum = sum(accepted_commerc,
#                                na.rm = TRUE),
#     accepted_commerc_mean = mean(accepted_commerc, 
#                                  na.rm = TRUE) %>% round(2))

vpo_roi_stats %>% 
  group_by(field,
           field_name,
           region, 
           type,
           year,
           group_code_name,
           edulvl,
           form) %>% 
  summarise(
    applied_budget_total_sum = sum(applied_budget_total, 
                                     na.rm = TRUE),
    applied_budget_total_mean = mean(applied_budget_total, 
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
                                        w = applied_budget_total,
                                        na.rm = TRUE) %>% round(2),
    konkurs_commerce_mean = weighted.mean(konkurs_commerc,
                                          w = applied_commerc,
                                          na.rm = TRUE) %>% round(2),
    ege_accepted_budget_mean = mean(ege_accepted_budget_only,
                                         na.rm = TRUE) %>% round(2),
    ege_accepted_commerc_mean = mean(ege_accepted_commerc,
                                     na.rm = TRUE) %>% round(2),
    ege_min_budget_mean = weighted.mean(ege_min_budget_only,
                                        w = ege_accepted_budget_only,
                                        na.rm = TRUE) %>% round(2),
    ege_min_commerc_mean = weighted.mean(ege_min_commerc,
                                         w = ege_accepted_commerc,
                                         na.rm = TRUE) %>% round(2),
    ege_mean_budget_mean = weighted.mean(ege_mean_budget_only,
                                        w = ege_accepted_budget_only,
                                        na.rm = TRUE) %>% round(2),
    ege_mean_commerc_mean = weighted.mean(ege_mean_commerc,
                                         w = ege_accepted_commerc,
                                         na.rm = TRUE) %>% round(2)
    ) %>% arrange(region, year, field) -> vpo_roi_avgs
  # sapply(function(x) sum(is.na(x)))

vpo_all_regions_stats %>% 
  group_by(field,
           field_name,
           region, 
           type,
           year,
           group_code_name,
           edulvl,
           form) %>% 
  summarise(
    applied_budget_total_sum = sum(applied_budget_total, 
                                   na.rm = TRUE),
    applied_budget_total_mean = mean(applied_budget_total, 
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
                                        w = applied_budget_total,
                                        na.rm = TRUE) %>% round(2),
    konkurs_commerce_mean = weighted.mean(konkurs_commerc,
                                          w = applied_commerc,
                                          na.rm = TRUE) %>% round(2),
    ege_accepted_budget_mean = mean(ege_accepted_budget_only,
                                    na.rm = TRUE) %>% round(2),
    ege_accepted_commerc_mean = mean(ege_accepted_commerc,
                                     na.rm = TRUE) %>% round(2),
    ege_min_budget_mean = weighted.mean(ege_min_budget_only,
                                        w = ege_accepted_budget_only,
                                        na.rm = TRUE) %>% round(2),
    ege_min_commerc_mean = weighted.mean(ege_min_commerc,
                                         w = ege_accepted_commerc,
                                         na.rm = TRUE) %>% round(2),
    ege_mean_budget_mean = weighted.mean(ege_mean_budget_only,
                                         w = ege_accepted_budget_only,
                                         na.rm = TRUE) %>% round(2),
    ege_mean_commerc_mean = weighted.mean(ege_mean_commerc,
                                          w = ege_accepted_commerc,
                                          na.rm = TRUE) %>% round(2)
  ) %>% arrange(region, year, field) -> vpo_allreg_avgs


colnames(roi_stats)


vpo_all_regions_stats %>% 
  group_by(field,
           field_name,
           region, 
           year) %>% 
  summarise(
    applied_budget_total_sum = sum(applied_budget_total, 
                                   na.rm = TRUE),
    accepted_budget_total_sum = sum(accepted_budget_total, 
                                    na.rm = TRUE),
    applied_commerc_sum = sum(applied_commerc, 
                              na.rm = TRUE),
    accepted_commerc_sum = sum(accepted_commerc,
                               na.rm = TRUE),
    konkurs_budget_mean = weighted.mean(konkurs_budget,
                                        w = applied_budget_total,
                                        na.rm = TRUE) %>% round(2),
    konkurs_commerce_mean = weighted.mean(konkurs_commerc,
                                          w = applied_commerc,
                                          na.rm = TRUE) %>% round(2)
  ) %>% 
  arrange(region, year, field) -> vpo_allreg_fields

save.image("vpo_export.RData")

# vpo_roi_avgs %>% #View()
#   googlesheets4::write_sheet(
#     ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
#     sheet = "VPO_groups"
#   )

# regions_ege %>% 
#   filter(region %in% c("Ростовская область", "Волгоградская область")) %>% 
#   pivot_longer(cols = c(konkurs_budget, konkurs_commerc),
#                values_to = "konkurs",
#                names_to = "basis") %>% 
#   ggplot(aes(x = year, 
#              y = konkurs,
#              linetype = basis, 
#              color = spec)) +
#   geom_line() +
#   facet_wrap(~ region, ncol = 1) +
#   guides(color = "none")
  






    
# (вместе учитывать однопрофильные и многопрофильные)
# 
# Отдельно по бюджету и отдельно по коммерции эти столбцы: 
#   сколько заявлений подано на приоритетные направления
# сколько было принято
# конкурс на место 
# средний балл ЕГЭ - только за счет бюджет. ассигнований (без доп квот)
# смотреть везде доли показателей от общих 
