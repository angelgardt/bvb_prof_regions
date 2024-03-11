library(tidyverse)
theme_set(theme_bw())
theme_update(legend.position = "bottom")

rm(list = ls())

spo_allreg_stats <- read_csv("spo_allreg_stats.csv")
spo_allreg_avgs <- read_csv("spo_allreg_avgs.csv")
spo_allreg_fields <- read_csv("spo_allreg_fields.csv")

vpo_allreg_stats <- read_csv("vpo_allreg_stats.csv")
vpo_allreg_avgs <- read_csv("vpo_allreg_avgs.csv")
vpo_allreg_fields <- read_csv("vpo_allreg_fields.csv")

roi <- c("Ростовская область", "Волгоградская область")

spo_allreg_avgs %>% #colnames()
  summarise(applied_budget_group_sum = sum(applied_budget_total_sum),
            applied_commerc_group_sum = sum(applied_commerc_sum),
            accepted_budget_group_sum = sum(accepted_budget_total_sum),
            accepted_commerc_group_sum = sum(accepted_commerc_sum),
            .by = c(region, year)) %>% 
  right_join(spo_allreg_avgs %>% ## without form, without type
               summarise(applied_budget_total_sum = sum(applied_budget_total_sum),
                         applied_commerc_sum = sum(applied_commerc_sum),
                         accepted_budget_total_sum = sum(accepted_budget_total_sum),
                         accepted_commerc_sum = sum(accepted_commerc_sum),
                         .by = c(region, year, group_code_name)),
             join_by(region, year)) %>% # View()
  mutate(perc_applied_budget = applied_budget_total_sum / applied_budget_group_sum,
         perc_applied_commerc = applied_commerc_sum / applied_commerc_group_sum,
         perc_accepted_budget = accepted_budget_total_sum / accepted_budget_group_sum,
         perc_accepted_commerc = accepted_commerc_sum / accepted_commerc_group_sum) %>% # print()
  select(region, 
         year,
         group_code_name,
         # type, ## without type
         # form, ## without form
         perc_applied_budget, 
         perc_applied_commerc,
         perc_accepted_budget,
         perc_accepted_commerc) -> spo_allreg_perc_applied_accepted

spo_allreg_perc_applied_accepted %>% 
  # sapply(function(x) sum(is.na(x)))
  filter(region %in% roi & 
           year %in% c(2016, 2017, 2022, 2023)) %>% 
  # filter(is.na(perc_accepted_commerc))
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  separate(group_code_name, into = c("group_code", "group_name"), sep = " - ") %>% 
  # filter(!str_detect(name, "konkurs")) %>% 
  mutate(
    group_code_short = group_code %>% str_remove("^\\d{1}\\.") %>% str_remove("\\.00\\.00$"),
    year = as_factor(year)
    ) %>% # pull(group_code_short)
  ggplot(aes(group_code_short, value, fill = year)) +
  geom_col(position = position_dodge()) +
  facet_grid(name ~ region, scales = "free_y",
             labeller = labeller(name = c(perc_applied_budget = "Поданных на бюджет", 
                                          perc_applied_commerc = "Поданных на коммерцию",
                                          perc_accepted_budget = "Принятых на бюджет",
                                          perc_accepted_commerc = "Принятых на коммерцию"))) +
  labs(x = "Код группы специальностей (x.XX.xx.xx)",
       y = "Доля от общего числа",
       fill = "Год",
       title = "Доля принятых/поданных заявлений от общего числа в отдельном регионе в 2016, 2022, 2023 гг.",
       subtitle = "СПО, без учета формы обучения и типа учебного заведения")


spo_allreg_perc_applied_accepted %>% 
  # sapply(function(x) sum(is.na(x)))
  filter(region %in% roi & 
           year %in% c(2016, 2017, 2022, 2023)) %>% 
  # filter(is.na(perc_accepted_commerc))
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  # separate(group_code_name, into = c("group_code", "group_name"), sep = " - ") %>% 
  # filter(!str_detect(name, "konkurs")) %>% 
  mutate(
    # group_code_short = group_code %>% str_remove("^\\d{1}\\.") %>% str_remove("\\.00\\.00$"),
    year = as_factor(year)
  ) %>% # pull(group_code_short)
  ggplot(aes(group_code_name, value, fill = year)) +
  geom_col(position = position_dodge()) +
  facet_grid(name ~ region, scales = "free_y",
             labeller = labeller(name = c(perc_applied_budget = "Поданных на бюджет", 
                                          perc_applied_commerc = "Поданных на коммерцию",
                                          perc_accepted_budget = "Принятых на бюджет",
                                          perc_accepted_commerc = "Принятых на коммерцию"))) +
  coord_flip() +
  labs(x = "Код группы специальностей (x.XX.xx.xx)",
       y = "Доля от общего числа",
       fill = "Год",
       title = "Доля принятых/поданных заявлений от общего числа в отдельном регионе в 2016, 2022, 2023 гг.",
       subtitle = "СПО, без учета формы обучения и типа учебного заведения")


spo_allreg_fields %>% # colnames()
  summarise(applied_budget_all = sum(applied_budget_total_sum),
            applied_commerc_all = sum(applied_commerc_sum),
            accepted_budget_all = sum(accepted_budget_total_sum),
            accepted_commerc_all = sum(accepted_commerc_sum),
            .by = c(region, year)) %>%
  right_join(spo_allreg_fields, ## without form, without type
             join_by(region, year)) %>% # View()
  mutate(perc_applied_budget = applied_budget_total_sum / applied_budget_all,
         perc_applied_commerc = applied_commerc_sum / applied_commerc_all,
         perc_accepted_budget = accepted_budget_total_sum / accepted_budget_all,
         perc_accepted_commerc = accepted_commerc_sum / accepted_commerc_all) %>% # print()
  select(region, 
         year,
         field,
         # group_code_name,
         # type, ## without type
         # form, ## without form
         perc_applied_budget, 
         perc_applied_commerc,
         perc_accepted_budget,
         perc_accepted_commerc) -> spo_allreg_fields_perc_applied_accepted

spo_allreg_fields_perc_applied_accepted %>% 
  filter(region %in% roi) %>% 
  pivot_longer(cols = -c(region, year, field)) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_grid(field ~ region)



spo_allreg_perc_applied_accepted %>% 
  # sapply(function(x) sum(is.na(x)))
  filter(region %in% roi) %>% 
  # filter(is.na(perc_accepted_commerc))
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  separate(group_code_name, into = c("group_code", "group_name"), sep = " - ") %>% 
  filter(!str_detect(name, "konkurs")) %>% 
  mutate(
    group_code_short = group_code %>% str_remove("^\\d{1}\\.") %>% str_remove("\\.00\\.00$"),
    year = as_factor(year)
  ) %>% # pull(group_code_short)
  ggplot(aes(year, value, color = name)) +
  geom_point() +
  facet_grid(group_code_short ~ region, scales = "free_y") +
  labs(x = "Год",
       y = "Доля от общего числа",
       fill = "Год",
       title = "Доля принятых/поданных заявлений от общего числа в отдельном регионе в 2016, 2022, 2023 гг.",
       subtitle = "СПО, без учета формы обучения и типа учебного заведения")








vpo_allreg_avgs %>% #colnames()
  summarise(applied_budget_group_sum = sum(applied_budget_total_sum),
            applied_commerc_group_sum = sum(applied_commerc_sum),
            accepted_budget_group_sum = sum(accepted_budget_total_sum),
            accepted_commerc_group_sum = sum(accepted_commerc_sum),
            .by = c(region, year)) %>% 
  right_join(vpo_allreg_avgs %>% ## without form, without type
               summarise(applied_budget_total_sum = sum(applied_budget_total_sum),
                         applied_commerc_sum = sum(applied_commerc_sum),
                         accepted_budget_total_sum = sum(accepted_budget_total_sum),
                         accepted_commerc_sum = sum(accepted_commerc_sum),
                         .by = c(region, year, group_code_name)),
             join_by(region, year)) %>% # View()
  mutate(perc_applied_budget = applied_budget_total_sum / applied_budget_group_sum,
         perc_applied_commerc = applied_commerc_sum / applied_commerc_group_sum,
         perc_accepted_budget = accepted_budget_total_sum / accepted_budget_group_sum,
         perc_accepted_commerc = accepted_commerc_sum / accepted_commerc_group_sum) %>% # print()
  select(region, 
         year,
         group_code_name,
         # type, ## without type
         # form, ## without form
         perc_applied_budget, 
         perc_applied_commerc,
         perc_accepted_budget,
         perc_accepted_commerc) -> vpo_allreg_perc_applied_accepted

vpo_allreg_perc_applied_accepted %>% 
  # sapply(function(x) sum(is.na(x)))
  filter(region %in% roi & 
           year %in% c(2016, 2022, 2023)) %>% 
  # filter(is.na(perc_accepted_commerc))
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  separate(group_code_name, into = c("group_code", "group_name"), sep = " - ") %>% 
  # filter(!str_detect(name, "konkurs")) %>% 
  mutate(group_code_short = group_code %>% str_remove("^\\d{1}\\.") %>% str_remove("\\.00\\.00$"),
         year = as_factor(year)) %>% # pull(group_code_short)
  ggplot(aes(group_code_short, value, fill = year)) +
  geom_col(position = position_dodge()) +
  facet_grid(name ~ region, scales = "free_y",
             labeller = labeller(name = c(perc_applied_budget = "Поданных на бюджет", 
                                          perc_applied_commerc = "Поданных на коммерцию",
                                          perc_accepted_budget = "Принятых на бюджет",
                                          perc_accepted_commerc = "Принятых на коммерцию"))) +
  labs(x = "Код группы специальностей (x.XX.xx.xx)",
       y = "Доля от общего числа",
       fill = "Год",
       title = "Доля принятых/поданных заявлений от общего числа в отдельном регионе в 2016, 2022, 2023 гг.",
       subtitle = "ВПО, без учета формы обучения и типа учебного заведения")




spo_allreg_avgs %>% #colnames()
  summarise(applied_budget_group_sum = sum(applied_budget_total_sum),
            applied_commerc_group_sum = sum(applied_commerc_sum),
            accepted_budget_group_sum = sum(accepted_budget_total_sum),
            accepted_commerc_group_sum = sum(accepted_commerc_sum),
            .by = c(region, year)) %>% 
  right_join(spo_allreg_avgs,
             join_by(region, year)) %>% # View()
  mutate(perc_applied_budget = applied_budget_total_sum / applied_budget_group_sum,
         perc_applied_commerc = applied_commerc_sum / applied_commerc_group_sum,
         perc_accepted_budget = accepted_budget_total_sum / accepted_budget_group_sum,
         perc_accepted_commerc = accepted_commerc_sum / accepted_commerc_group_sum) %>% # print()
  select(region, 
         year,
         group_code_name,
         type,
         form,
         perc_applied_budget, 
         perc_applied_commerc,
         perc_accepted_budget,
         perc_accepted_commerc) -> spo_allreg_perc_applied_accepted_type_form

spo_allreg_perc_applied_accepted_type_form %>% 
  # sapply(function(x) sum(is.na(x)))
  filter(region %in% roi & 
           year %in% c(2016, 2022, 2023) & 
           type == "Государственные") %>% 
  # filter(is.na(perc_accepted_commerc))
  pivot_longer(cols = -c(region, year, form, type, group_code_name)) %>% 
  separate(group_code_name, into = c("group_code", "group_name"), sep = " - ") %>% 
  # filter(!str_detect(name, "konkurs")) %>% 
  mutate(group_code_short = group_code %>% str_remove("^\\d{1}\\.") %>% str_remove("\\.00\\.00$"),
         year = as_factor(year)) %>% # pull(group_code_short)
  ggplot(aes(group_code_short, value, fill = year)) +
  geom_col(position = position_dodge()) +
  facet_grid(name ~ region + form, scales = "free",
             labeller = labeller(name = c(perc_applied_budget = "Поданных на бюджет", 
                                          perc_applied_commerc = "Поданных на коммерцию",
                                          perc_accepted_budget = "Принятых на бюджет",
                                          perc_accepted_commerc = "Принятых на коммерцию"))) +
  labs(x = "Код группы специальностей (x.XX.xx.xx)",
       y = "Доля от общего числа",
       fill = "Год",
       title = "Доля принятых/поданных заявлений от общего числа в отдельном регионе в 2016, 2022, 2023 гг.",
       subtitle = "СПО, государственные, c учетом формы обучения")


spo_allreg_perc_applied_accepted_type_form %>% 
  # sapply(function(x) sum(is.na(x)))
  filter(region %in% roi & 
           year %in% c(2016, 2022, 2023) & 
           type == "Негосударственные") %>% 
  # filter(is.na(perc_accepted_commerc))
  pivot_longer(cols = -c(region, year, form, type, group_code_name)) %>% 
  separate(group_code_name, into = c("group_code", "group_name"), sep = " - ") %>% 
  # filter(!str_detect(name, "konkurs")) %>% 
  mutate(group_code_short = group_code %>% str_remove("^\\d{1}\\.") %>% str_remove("\\.00\\.00$"),
         year = as_factor(year)) %>% # pull(group_code_short)
  ggplot(aes(group_code_short, value, fill = year)) +
  geom_col(position = position_dodge()) +
  facet_grid(name ~ region + form, scales = "free",
             labeller = labeller(name = c(perc_applied_budget = "Поданных на бюджет", 
                                          perc_applied_commerc = "Поданных на коммерцию",
                                          perc_accepted_budget = "Принятых на бюджет",
                                          perc_accepted_commerc = "Принятых на коммерцию"))) +
  labs(x = "Код группы специальностей (x.XX.xx.xx)",
       y = "Доля от общего числа",
       fill = "Год",
       title = "Доля принятых/поданных заявлений от общего числа в отдельном регионе в 2016, 2022, 2023 гг.",
       subtitle = "СПО, негосударственные, c учетом формы обучения")




vpo_allreg_avgs %>% #colnames()
  summarise(applied_budget_group_sum = sum(applied_budget_total_sum),
            applied_commerc_group_sum = sum(applied_commerc_sum),
            accepted_budget_group_sum = sum(accepted_budget_total_sum),
            accepted_commerc_group_sum = sum(accepted_commerc_sum),
            .by = c(region, year)) %>% 
  right_join(spo_allreg_avgs,
             join_by(region, year)) %>% # View()
  mutate(perc_applied_budget = applied_budget_total_sum / applied_budget_group_sum,
         perc_applied_commerc = applied_commerc_sum / applied_commerc_group_sum,
         perc_accepted_budget = accepted_budget_total_sum / accepted_budget_group_sum,
         perc_accepted_commerc = accepted_commerc_sum / accepted_commerc_group_sum) %>% # print()
  select(region, 
         year,
         group_code_name,
         type,
         form,
         perc_applied_budget, 
         perc_applied_commerc,
         perc_accepted_budget,
         perc_accepted_commerc) -> vpo_allreg_perc_applied_accepted_type_form

vpo_allreg_perc_applied_accepted_type_form %>% 
  # sapply(function(x) sum(is.na(x)))
  filter(region %in% roi & 
           year %in% c(2016, 2022, 2023) & 
           type == "Государственные") %>% 
  # filter(is.na(perc_accepted_commerc))
  pivot_longer(cols = -c(region, year, form, type, group_code_name)) %>% 
  separate(group_code_name, into = c("group_code", "group_name"), sep = " - ") %>% 
  # filter(!str_detect(name, "konkurs")) %>% 
  mutate(group_code_short = group_code %>% str_remove("^\\d{1}\\.") %>% str_remove("\\.00\\.00$"),
         year = as_factor(year)) %>% # pull(group_code_short)
  ggplot(aes(group_code_short, value, fill = year)) +
  geom_col(position = position_dodge()) +
  facet_grid(name ~ region + form, scales = "free",
             labeller = labeller(name = c(perc_applied_budget = "Поданных на бюджет", 
                                          perc_applied_commerc = "Поданных на коммерцию",
                                          perc_accepted_budget = "Принятых на бюджет",
                                          perc_accepted_commerc = "Принятых на коммерцию"))) +
  labs(x = "Код группы специальностей (x.XX.xx.xx)",
       y = "Доля от общего числа",
       fill = "Год",
       title = "Доля принятых/поданных заявлений от общего числа в отдельном регионе в 2016, 2022, 2023 гг.",
       subtitle = "ВПО, государственные, c учетом формы обучения")


vpo_allreg_perc_applied_accepted_type_form %>% 
  # sapply(function(x) sum(is.na(x)))
  filter(region %in% roi & 
           year %in% c(2016, 2022, 2023) & 
           type == "Негосударственные") %>% 
  # filter(is.na(perc_accepted_commerc))
  pivot_longer(cols = -c(region, year, form, type, group_code_name)) %>% 
  separate(group_code_name, into = c("group_code", "group_name"), sep = " - ") %>% 
  # filter(!str_detect(name, "konkurs")) %>% 
  mutate(group_code_short = group_code %>% str_remove("^\\d{1}\\.") %>% str_remove("\\.00\\.00$"),
         year = as_factor(year)) %>% # pull(group_code_short)
  ggplot(aes(group_code_short, value, fill = year)) +
  geom_col(position = position_dodge()) +
  facet_grid(name ~ region + form, scales = "free",
             labeller = labeller(name = c(perc_applied_budget = "Поданных на бюджет", 
                                          perc_applied_commerc = "Поданных на коммерцию",
                                          perc_accepted_budget = "Принятых на бюджет",
                                          perc_accepted_commerc = "Принятых на коммерцию"))) +
  labs(x = "Код группы специальностей (x.XX.xx.xx)",
       y = "Доля от общего числа",
       fill = "Год",
       title = "Доля принятых/поданных заявлений от общего числа в отдельном регионе в 2016, 2022, 2023 гг.",
       subtitle = "ВПО, негосударственные, c учетом формы обучения")


spo_allreg_stats %>% 
  distinct(form, group_code_name) %>% 
  summarise(n_group_spo = n(),
            .by = form) %>% 
  full_join(
    spo_allreg_stats %>% 
      distinct(form, code) %>% 
      summarise(n_spec_spo = n(),
                .by = form)
  ) %>% 
  full_join(vpo_allreg_stats %>% 
              distinct(form, group_code_name) %>% 
              summarise(n_group_vpo = n(),
                        .by = form)) %>% 
  full_join(vpo_allreg_stats %>% 
              distinct(form, code) %>% 
              summarise(n_spec_vpo = n(),
                        .by = form))

spo_allreg_avgs %>% # colnames()
  separate(group_code_name, into = c("group_code", "group_name"), sep = " - ") %>% 
  mutate(group_code_short = group_code %>% str_remove("^\\d{1}\\.") %>% str_remove("\\.00\\.00$"),
         year = as_factor(year)) %>% # pull(group_code_short)
  select(konkurs_budget_mean, 
         konkurs_commerce_mean,
         group_code_short,
         type,
         form,
         region,
         year) %>% 
  pivot_longer(cols = c(konkurs_budget_mean, konkurs_commerce_mean)) %>% 
  filter(region %in% roi & 
           year %in% c(2016, 2022, 2023)) %>% 
  ggplot(aes(group_code_short, value, fill = year)) +
  geom_col(position = position_dodge()) +
  facet_grid(
    name ~ region,
    # name ~ region + form + type, 
    scales = "free_y",
    labeller = labeller(name = c(konkurs_budget_mean = "Бюджет", 
                                 konkurs_commerce_mean = "Коммерция"))) +
  labs(x = "Код группы специальностей (x.XX.xx.xx)",
       y = "Средний конкурс (взвешенный на количество поданных заявлений)",
       fill = "Год",
       title = "Конкурс на группы специальностей в 2016, 2022, 2023 гг.",
       subtitle = "СПО, без учета формы обучения и типа учебного заведения")
  

vpo_allreg_avgs %>% # colnames()
  separate(group_code_name, into = c("group_code", "group_name"), sep = " - ") %>% 
  mutate(group_code_short = group_code %>% str_remove("^\\d{1}\\.") %>% str_remove("\\.00\\.00$"),
         year = as_factor(year)) %>% # pull(group_code_short)
  select(konkurs_budget_mean, 
         konkurs_commerce_mean,
         group_code_short,
         type,
         form,
         region,
         year) %>% 
  pivot_longer(cols = c(konkurs_budget_mean, konkurs_commerce_mean)) %>% 
  filter(region %in% roi & 
           year %in% c(2016, 2022, 2023)) %>% 
  ggplot(aes(group_code_short, value, fill = year)) +
  geom_col(position = position_dodge()) +
  facet_grid(
    name ~ region,
    # name ~ region + form + type, 
    scales = "free_y",
    labeller = labeller(name = c(konkurs_budget_mean = "Бюджет", 
                                 konkurs_commerce_mean = "Коммерция"))) +
  labs(x = "Код группы специальностей (x.XX.xx.xx)",
       y = "Средний конкурс (взвешенный на количество поданных заявлений)",
       fill = "Год",
       title = "Конкурс на группы специальностей в 2016, 2022, 2023 гг.",
       subtitle = "ВПО, без учета формы обучения и типа учебного заведения")




vpo_allreg_fields %>% # colnames()
  summarise(applied_budget_all = sum(applied_budget_total_sum),
            applied_commerc_all = sum(applied_commerc_sum),
            accepted_budget_all = sum(accepted_budget_total_sum),
            accepted_commerc_all = sum(accepted_commerc_sum),
            .by = c(region, year)) %>%
  right_join(vpo_allreg_fields, ## without form, without type
             join_by(region, year)) %>% # View()
  mutate(perc_applied_budget = applied_budget_total_sum / applied_budget_all,
         perc_applied_commerc = applied_commerc_sum / applied_commerc_all,
         perc_accepted_budget = accepted_budget_total_sum / accepted_budget_all,
         perc_accepted_commerc = accepted_commerc_sum / accepted_commerc_all) %>% # print()
  select(region, 
         year,
         field,
         # group_code_name,
         # type, ## without type
         # form, ## without form
         perc_applied_budget, 
         perc_applied_commerc,
         perc_accepted_budget,
         perc_accepted_commerc) -> vpo_allreg_fields_perc_applied_accepted



vpo_allreg_fields_perc_applied_accepted %>% 
  filter(region %in% roi) %>% 
  pivot_longer(cols = -c(region, year, field)) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_grid(field ~ region) +
  scale_color_discrete(labels = c(perc_applied_budget = "Поданные, бюджет", 
                                  perc_applied_commerc = "Поданные, коммерция",
                                  perc_accepted_budget = "Принятые, бюджет",
                                  perc_accepted_commerc = "Принятые, коммерция")) +
  labs(x = "Год", y = "Доля",
       color = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "ВПО")

spo_allreg_fields_perc_applied_accepted %>% 
  filter(region %in% roi) %>% 
  pivot_longer(cols = -c(region, year, field)) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_grid(field ~ region) +
  scale_color_discrete(labels = c(perc_applied_budget = "Поданные, бюджет", 
                                  perc_applied_commerc = "Поданные, коммерция",
                                  perc_accepted_budget = "Принятые, бюджет",
                                  perc_accepted_commerc = "Принятые, коммерция")) +
  labs(x = "Год", y = "Доля",
       color = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "CПО")



spo_allreg_avgs %>% #colnames()
  summarise(applied_budget_group_sum = sum(applied_budget_total_sum),
            applied_commerc_group_sum = sum(applied_commerc_sum),
            accepted_budget_group_sum = sum(accepted_budget_total_sum),
            accepted_commerc_group_sum = sum(accepted_commerc_sum),
            .by = c(region, year)) %>% 
  right_join(spo_allreg_avgs %>% ## without form, without type
               summarise(applied_budget_total_sum = sum(applied_budget_total_sum),
                         applied_commerc_sum = sum(applied_commerc_sum),
                         accepted_budget_total_sum = sum(accepted_budget_total_sum),
                         accepted_commerc_sum = sum(accepted_commerc_sum),
                         .by = c(region, year, group_code_name)),
             join_by(region, year)) %>% # View()
  mutate(perc_applied_budget = applied_budget_total_sum / applied_budget_group_sum,
         perc_applied_commerc = applied_commerc_sum / applied_commerc_group_sum,
         perc_accepted_budget = accepted_budget_total_sum / accepted_budget_group_sum,
         perc_accepted_commerc = accepted_commerc_sum / accepted_commerc_group_sum) %>% # print()
  select(region, 
         year,
         group_code_name,
         # type, ## without type
         # form, ## without form
         perc_applied_budget, 
         perc_applied_commerc,
         perc_accepted_budget,
         perc_accepted_commerc)
  

## SPO 1
spo_allreg_perc_applied_accepted %>% 
  filter(region %in% roi &
           str_detect(group_code_name, "^1")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_grid(code ~ region,
             scales = "free_y") +
  scale_color_discrete(labels = c(perc_applied_budget = "Поданные, бюджет", 
                                  perc_applied_commerc = "Поданные, коммерция",
                                  perc_accepted_budget = "Принятые, бюджет",
                                  perc_accepted_commerc = "Принятые, коммерция")) +
  labs(x = "Год", y = "Доля",
       color = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "CПО, Математические и естественные науки")


## SPO 2
spo_allreg_perc_applied_accepted %>% 
  filter(region %in% roi &
           str_detect(group_code_name, "^2")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_grid(code ~ region,
             scales = "free_y") +
  scale_color_discrete(labels = c(perc_applied_budget = "Поданные, бюджет", 
                                  perc_applied_commerc = "Поданные, коммерция",
                                  perc_accepted_budget = "Принятые, бюджет",
                                  perc_accepted_commerc = "Принятые, коммерция")) +
  labs(x = "Год", y = "Доля",
       color = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "CПО, Инженерное дело, технологии и технические науки")


## SPO 3
spo_allreg_perc_applied_accepted %>% 
  filter(region %in% roi &
           str_detect(group_code_name, "^3")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_grid(code ~ region,
             scales = "free_y") +
  scale_color_discrete(labels = c(perc_applied_budget = "Поданные, бюджет", 
                                  perc_applied_commerc = "Поданные, коммерция",
                                  perc_accepted_budget = "Принятые, бюджет",
                                  perc_accepted_commerc = "Принятые, коммерция")) +
  labs(x = "Год", y = "Доля",
       color = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "CПО, Здравоохранение и медицинские науки")


## SPO 4
spo_allreg_perc_applied_accepted %>% 
  filter(region %in% roi &
           str_detect(group_code_name, "^4")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_grid(code ~ region,
             scales = "free_y") +
  scale_color_discrete(labels = c(perc_applied_budget = "Поданные, бюджет", 
                                  perc_applied_commerc = "Поданные, коммерция",
                                  perc_accepted_budget = "Принятые, бюджет",
                                  perc_accepted_commerc = "Принятые, коммерция")) +
  labs(x = "Год", y = "Доля",
       color = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "CПО, Сельское хозяйство и сельскохозяйственные науки")

## SPO 5
spo_allreg_perc_applied_accepted %>% 
  filter(region %in% roi &
           str_detect(group_code_name, "^5")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_grid(code ~ region,
             scales = "free_y") +
  scale_color_discrete(labels = c(perc_applied_budget = "Поданные, бюджет", 
                                  perc_applied_commerc = "Поданные, коммерция",
                                  perc_accepted_budget = "Принятые, бюджет",
                                  perc_accepted_commerc = "Принятые, коммерция")) +
  labs(x = "Год", y = "Доля",
       color = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "CПО, Науки об обществе")


## SPO 6
spo_allreg_perc_applied_accepted %>% 
  filter(region %in% roi &
           str_detect(group_code_name, "^6")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_grid(code ~ region,
             scales = "free_y") +
  scale_color_discrete(labels = c(perc_applied_budget = "Поданные, бюджет", 
                                  perc_applied_commerc = "Поданные, коммерция",
                                  perc_accepted_budget = "Принятые, бюджет",
                                  perc_accepted_commerc = "Принятые, коммерция")) +
  labs(x = "Год", y = "Доля",
       color = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "CПО, Образование и педагогические науки")


## SPO 7
spo_allreg_perc_applied_accepted %>% 
  filter(region %in% roi &
           str_detect(group_code_name, "^7")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_grid(code ~ region,
             scales = "free_y") +
  scale_color_discrete(labels = c(perc_applied_budget = "Поданные, бюджет", 
                                  perc_applied_commerc = "Поданные, коммерция",
                                  perc_accepted_budget = "Принятые, бюджет",
                                  perc_accepted_commerc = "Принятые, коммерция")) +
  labs(x = "Год", y = "Доля",
       color = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "CПО, Гуманитарные науки")


## SPO 8
spo_allreg_perc_applied_accepted %>% 
  filter(region %in% roi &
           str_detect(group_code_name, "^8")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_grid(code ~ region,
             scales = "free_y") +
  scale_color_discrete(labels = c(perc_applied_budget = "Поданные, бюджет", 
                                  perc_applied_commerc = "Поданные, коммерция",
                                  perc_accepted_budget = "Принятые, бюджет",
                                  perc_accepted_commerc = "Принятые, коммерция")) +
  labs(x = "Год", y = "Доля",
       color = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "CПО, Искусство и культура")



## VPO 1
vpo_allreg_perc_applied_accepted %>% 
  filter(region %in% roi &
           str_detect(group_code_name, "^1")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_grid(code ~ region,
             scales = "free_y") +
  scale_color_discrete(labels = c(perc_applied_budget = "Поданные, бюджет", 
                                  perc_applied_commerc = "Поданные, коммерция",
                                  perc_accepted_budget = "Принятые, бюджет",
                                  perc_accepted_commerc = "Принятые, коммерция")) +
  labs(x = "Год", y = "Доля",
       color = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "ВПО, Математические и естественные науки")


## VPO 2
vpo_allreg_perc_applied_accepted %>% 
  filter(region %in% roi &
           str_detect(group_code_name, "^2")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_grid(code ~ region,
             scales = "free_y") +
  scale_color_discrete(labels = c(perc_applied_budget = "Поданные, бюджет", 
                                  perc_applied_commerc = "Поданные, коммерция",
                                  perc_accepted_budget = "Принятые, бюджет",
                                  perc_accepted_commerc = "Принятые, коммерция")) +
  labs(x = "Год", y = "Доля",
       color = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "ВПО, Инженерное дело, технологии и технические науки")


## VPO 3
vpo_allreg_perc_applied_accepted %>% 
  filter(region %in% roi &
           str_detect(group_code_name, "^3")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_grid(code ~ region,
             scales = "free_y") +
  scale_color_discrete(labels = c(perc_applied_budget = "Поданные, бюджет", 
                                  perc_applied_commerc = "Поданные, коммерция",
                                  perc_accepted_budget = "Принятые, бюджет",
                                  perc_accepted_commerc = "Принятые, коммерция")) +
  labs(x = "Год", y = "Доля",
       color = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "ВПО, Здравоохранение и медицинские науки")


## VPO 4
vpo_allreg_perc_applied_accepted %>% 
  filter(region %in% roi &
           str_detect(group_code_name, "^4")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_grid(code ~ region,
             scales = "free_y") +
  scale_color_discrete(labels = c(perc_applied_budget = "Поданные, бюджет", 
                                  perc_applied_commerc = "Поданные, коммерция",
                                  perc_accepted_budget = "Принятые, бюджет",
                                  perc_accepted_commerc = "Принятые, коммерция")) +
  labs(x = "Год", y = "Доля",
       color = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "ВПО, Сельское хозяйство и сельскохозяйственные науки")

## VPO 5
vpo_allreg_perc_applied_accepted %>% 
  filter(region %in% roi &
           str_detect(group_code_name, "^5")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_grid(code ~ region,
             scales = "free_y") +
  scale_color_discrete(labels = c(perc_applied_budget = "Поданные, бюджет", 
                                  perc_applied_commerc = "Поданные, коммерция",
                                  perc_accepted_budget = "Принятые, бюджет",
                                  perc_accepted_commerc = "Принятые, коммерция")) +
  labs(x = "Год", y = "Доля",
       color = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "ВПО, Науки об обществе")


## VPO 6
vpo_allreg_perc_applied_accepted %>% 
  filter(region %in% roi &
           str_detect(group_code_name, "^6")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_grid(code ~ region,
             scales = "free_y") +
  scale_color_discrete(labels = c(perc_applied_budget = "Поданные, бюджет", 
                                  perc_applied_commerc = "Поданные, коммерция",
                                  perc_accepted_budget = "Принятые, бюджет",
                                  perc_accepted_commerc = "Принятые, коммерция")) +
  labs(x = "Год", y = "Доля",
       color = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "ВПО, Образование и педагогические науки")


## VPO 7
vpo_allreg_perc_applied_accepted %>% 
  filter(region %in% roi &
           str_detect(group_code_name, "^7")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_grid(code ~ region,
             scales = "free_y") +
  scale_color_discrete(labels = c(perc_applied_budget = "Поданные, бюджет", 
                                  perc_applied_commerc = "Поданные, коммерция",
                                  perc_accepted_budget = "Принятые, бюджет",
                                  perc_accepted_commerc = "Принятые, коммерция")) +
  labs(x = "Год", y = "Доля",
       color = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "ВПО, Гуманитарные науки")


## VPO 8
vpo_allreg_perc_applied_accepted %>% 
  filter(region %in% roi &
           str_detect(group_code_name, "^8")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_grid(code ~ region,
             scales = "free_y") +
  scale_color_discrete(labels = c(perc_applied_budget = "Поданные, бюджет", 
                                  perc_applied_commerc = "Поданные, коммерция",
                                  perc_accepted_budget = "Принятые, бюджет",
                                  perc_accepted_commerc = "Принятые, коммерция")) +
  labs(x = "Год", y = "Доля",
       color = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "ВПО, Искусство и культура")

