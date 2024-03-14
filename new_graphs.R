library(tidyverse)
theme_set(theme_bw())
theme_update(legend.position = "bottom")

rm(list = ls())

### ----
spo_allreg_stats <- read_csv("spo_allreg_stats.csv")
spo_allreg_avgs <- read_csv("spo_allreg_avgs.csv")
spo_allreg_fields <- read_csv("spo_allreg_fields.csv")

vpo_allreg_stats <- read_csv("vpo_allreg_stats.csv")
vpo_allreg_avgs <- read_csv("vpo_allreg_avgs.csv")
vpo_allreg_fields <- read_csv("vpo_allreg_fields.csv")

roi_pfo <- c("Нижегородская область", "Самарская область")
roi_usfo <- c("Тюменская область", "Омская область")
roi_ufo <- c("Ростовская область", "Волгоградская область")


spo_allreg_avgs %>% #colnames()
  summarise(applied_budget_group_sum = sum(applied_budget_total_sum),
            applied_commerc_group_sum = sum(applied_commerc_sum),
            accepted_budget_group_sum = sum(accepted_budget_total_sum),
            accepted_commerc_group_sum = sum(accepted_commerc_sum),
            .by = c(region, year)) %>% # print() 
  mutate(applied_sum = applied_budget_group_sum + applied_commerc_group_sum,
         accepted_sum = accepted_budget_group_sum + accepted_commerc_group_sum) %>% 
  select(region, year, accepted_sum, applied_sum) %>% 
  right_join(
    spo_allreg_avgs %>% ## without form, without type
      summarise(applied_budget_total_sum = sum(applied_budget_total_sum),
                applied_commerc_sum = sum(applied_commerc_sum),
                accepted_budget_total_sum = sum(accepted_budget_total_sum),
                accepted_commerc_sum = sum(accepted_commerc_sum),
                .by = c(region, year, group_code_name)) %>% 
      mutate(applied_group_sum = applied_budget_total_sum + applied_commerc_sum,
             accepted_group_sum = accepted_budget_total_sum + accepted_commerc_sum) %>% 
      select(region, year, group_code_name, accepted_group_sum, applied_group_sum),
    join_by(region, year)) %>% # print()
  mutate(perc_applied = applied_group_sum / applied_sum,
         perc_accepted = accepted_group_sum / accepted_sum) -> spo_allreg_perc_applied_accepted


spo_allreg_fields %>% # colnames()
  summarise(applied_budget_all = sum(applied_budget_total_sum),
            applied_commerc_all = sum(applied_commerc_sum),
            accepted_budget_all = sum(accepted_budget_total_sum),
            accepted_commerc_all = sum(accepted_commerc_sum),
            .by = c(region, year)) %>%
  mutate(applied_sum = applied_budget_all + applied_commerc_all,
         accepted_sum = accepted_budget_all + accepted_commerc_all) %>% 
  select(region, year, accepted_sum, applied_sum) %>% 
  right_join(spo_allreg_fields %>% 
               mutate(applied_field = applied_budget_total_sum + applied_commerc_sum,
                      accepted_field = accepted_budget_total_sum + accepted_commerc_sum) %>% 
               select(region, year, field, field_name, applied_field, accepted_field), ## without form, without type
             join_by(region, year)) %>% # print()
  mutate(perc_applied = applied_field / applied_sum,
         perc_accepted = accepted_field / accepted_sum) -> spo_allreg_fields_perc_applied_accepted

vpo_allreg_avgs %>% #colnames()
  summarise(applied_budget_group_sum = sum(applied_budget_total_sum),
            applied_commerc_group_sum = sum(applied_commerc_sum),
            accepted_budget_group_sum = sum(accepted_budget_total_sum),
            accepted_commerc_group_sum = sum(accepted_commerc_sum),
            .by = c(region, year)) %>%
  mutate(applied_sum = applied_budget_group_sum + applied_commerc_group_sum,
         accepted_sum = accepted_budget_group_sum + accepted_commerc_group_sum) %>% 
  select(region, year, accepted_sum, applied_sum) %>% 
  right_join(vpo_allreg_avgs %>% ## without form, without type
               summarise(applied_budget_total_sum = sum(applied_budget_total_sum),
                         applied_commerc_sum = sum(applied_commerc_sum),
                         accepted_budget_total_sum = sum(accepted_budget_total_sum),
                         accepted_commerc_sum = sum(accepted_commerc_sum),
                         .by = c(region, year, group_code_name)) %>% 
               mutate(applied_group_sum = applied_budget_total_sum + applied_commerc_sum,
                      accepted_group_sum = accepted_budget_total_sum + accepted_commerc_sum) %>% 
               select(region, year, group_code_name, accepted_group_sum, applied_group_sum),
             join_by(region, year)) %>% # View()
  mutate(perc_applied = applied_group_sum / applied_sum,
         perc_accepted = accepted_group_sum / accepted_sum) -> vpo_allreg_perc_applied_accepted

vpo_allreg_fields %>% # colnames()
  summarise(applied_budget_all = sum(applied_budget_total_sum),
            applied_commerc_all = sum(applied_commerc_sum),
            accepted_budget_all = sum(accepted_budget_total_sum),
            accepted_commerc_all = sum(accepted_commerc_sum),
            .by = c(region, year)) %>%
  mutate(applied_sum = applied_budget_all + applied_commerc_all,
         accepted_sum = accepted_budget_all + accepted_commerc_all) %>% 
  select(region, year, accepted_sum, applied_sum) %>% 
  right_join(vpo_allreg_fields %>% 
             mutate(applied_field = applied_budget_total_sum + applied_commerc_sum,
                    accepted_field = accepted_budget_total_sum + accepted_commerc_sum) %>% 
               select(region, year, field, field_name, applied_field, accepted_field), ## without form, without type
             join_by(region, year)) %>% # print()
  mutate(perc_applied = applied_field / applied_sum,
         perc_accepted = accepted_field / accepted_sum) -> vpo_allreg_fields_perc_applied_accepted


### UFO -----

spo_allreg_fields_perc_applied_accepted %>% 
  filter(region %in% roi_ufo) %>% 
  pivot_longer(cols = -c(region, year, field, field_name)) %>% 
  filter(str_detect(name, "perc")) %>% 
  mutate(value = value * 100) %>% 
  ggplot(aes(year, value, linetype = name, shape = name)) +
  geom_line() +
  geom_point(size = 2) +
  facet_grid(field ~ region) +
  scale_linetype_manual(values = c(perc_applied = "solid",
                                   perc_accepted = "dashed"),
                        labels = c(perc_applied = "Подано",
                                   perc_accepted = "Принято")) +
  scale_shape_manual(values = c(perc_applied = 16,
                                perc_accepted = 17),
                     labels = c(perc_applied = "Подано",
                                perc_accepted = "Принято")) +
  labs(x = "Год", y = "% от общего числа",
       linetype = "",
       shape = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "CПО")


vpo_allreg_fields_perc_applied_accepted %>% 
  filter(region %in% roi_ufo) %>% 
  pivot_longer(cols = -c(region, year, field, field_name)) %>% 
  filter(str_detect(name, "perc")) %>% 
  mutate(value = value * 100) %>% 
  ggplot(aes(year, value, linetype = name, shape = name)) +
  geom_line() +
  geom_point(size = 2) +
  facet_grid(field ~ region) +
  scale_linetype_manual(values = c(perc_applied = "solid",
                                   perc_accepted = "dashed"),
                        labels = c(perc_applied = "Подано",
                                   perc_accepted = "Принято")) +
  scale_shape_manual(values = c(perc_applied = 16,
                                perc_accepted = 17),
                     labels = c(perc_applied = "Подано",
                                perc_accepted = "Принято")) +
  labs(x = "Год", y = "% от общего числа",
       linetype = "",
       shape = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "ВПО")








spo_allreg_fields %>% 
  distinct(field, field_name) %>% 
  arrange(field) -> fields

rois <- list(roi_ufo, roi_pfo, roi_usfo)


for (i in 1:nrow(fields)) {
  for (roi in rois) {
    print(roi)
    print(i)
    
    if (spo_allreg_perc_applied_accepted %>%
        filter(region %in% roi &
               str_detect(group_code_name, paste0("^", i))) %>%
        nrow() == 0)
      next
    
    spo_allreg_perc_applied_accepted %>%
      filter(region %in% roi &
               str_detect(group_code_name, paste0("^", i))) %>%
      pivot_longer(cols = -c(region, year, group_code_name)) %>%
      filter(str_detect(name, "perc")) %>%
      mutate(
        code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$"),
        value = value * 100
      ) %>%
      ggplot(aes(year, value, linetype = name, shape = name)) +
      geom_line() +
      geom_point(size = 2) +
      facet_grid(code ~ region,
                 scales = "free_y") +
      scale_linetype_manual(values = c(perc_applied = "solid",
                                       perc_accepted = "dashed"),
                            labels = c(perc_applied = "Подано",
                                       perc_accepted = "Принято")) +
      scale_shape_manual(values = c(perc_applied = 16,
                                    perc_accepted = 17),
                         labels = c(perc_applied = "Подано",
                                    perc_accepted = "Принято")) +
      labs(
        x = "Год",
        y = "% от общего числа",
        linetype = "",
        shape = "",
        title = "Динамика поданных/принятых заявлений",
        subtitle = paste0("CПО, ", fields$field_name[i])
      ) -> current_spo
    
    if (vpo_allreg_perc_applied_accepted %>%
        filter(region %in% roi &
               str_detect(group_code_name, paste0("^", i))) %>%
        nrow() == 0)
      next
    
    vpo_allreg_perc_applied_accepted %>%
      filter(region %in% roi &
               str_detect(group_code_name, paste0("^", i))) %>%
      pivot_longer(cols = -c(region, year, group_code_name)) %>%
      filter(str_detect(name, "perc")) %>%
      mutate(
        code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$"),
        value = value * 100
      ) %>%
      ggplot(aes(year, value, linetype = name, shape = name)) +
      geom_line() +
      geom_point(size = 2) +
      facet_grid(code ~ region,
                 scales = "free_y") +
      scale_linetype_manual(values = c(perc_applied = "solid",
                                       perc_accepted = "dashed"),
                            labels = c(perc_applied = "Подано",
                                       perc_accepted = "Принято")) +
      scale_shape_manual(values = c(perc_applied = 16,
                                    perc_accepted = 17),
                         labels = c(perc_applied = "Подано",
                                    perc_accepted = "Принято")) +
      labs(
        x = "Год",
        y = "% от общего числа",
        linetype = "",
        shape = "",
        title = "Динамика поданных/принятых заявлений",
        subtitle = paste0("ВПО, ", fields$field_name[i])
      ) -> current_vpo
    
    print(current_spo)
    print(current_vpo)
  }
}


## SPO 1
spo_allreg_perc_applied_accepted %>% 
  filter(region %in% roi_ufo &
           str_detect(group_code_name, "^1")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  filter(str_detect(name, "perc")) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  ggplot(aes(year, value, linetype = name, shape = name)) +
  geom_line() +
  geom_point() +
  facet_grid(code ~ region,
             scales = "free_y") +
  scale_color_discrete(labels = c(perc_applied = "Поданные, бюджет",
                                  perc_accepted = "Принятые, коммерция")) +
  labs(x = "Год", y = "% от общего числа",
       color = "",
       title = "Динамика поданных/принятых заявлений",
       subtitle = "CПО, Математические и естественные науки")


## SPO 2
spo_allreg_perc_applied_accepted %>% 
  filter(region %in% roi_ufo &
           str_detect(group_code_name, "^2")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  filter(str_detect(name, "perc")) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, linetype = name, shape = name)) +
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
  filter(region %in% roi_ufo &
           str_detect(group_code_name, "^3")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  filter(str_detect(name, "perc")) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, linetype = name, shape = name)) +
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
  filter(region %in% roi_ufo &
           str_detect(group_code_name, "^4")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  filter(str_detect(name, "perc")) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, linetype = name, shape = name)) +
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
  filter(region %in% roi_ufo &
           str_detect(group_code_name, "^5")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  filter(str_detect(name, "perc")) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, linetype = name, shape = name)) +
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
  filter(region %in% roi_ufo &
           str_detect(group_code_name, "^6")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  filter(str_detect(name, "perc")) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, linetype = name, shape = name)) +
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
  filter(region %in% roi_ufo &
           str_detect(group_code_name, "^7")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  filter(!str_detect(name, "konkurs")) %>% # print()
  ggplot(aes(year, value, linetype = name, shape = name)) +
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
  filter(region %in% roi_ufo &
           str_detect(group_code_name, "^8")) %>% 
  pivot_longer(cols = -c(region, year, group_code_name)) %>% 
  mutate(code = group_code_name %>% str_remove_all("^\\d{1}\\.") %>% str_remove_all("\\.\\d{2}.+$")) %>% 
  ggplot(aes(year, value, linetype = name, shape = name)) +
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




