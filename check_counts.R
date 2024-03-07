library(tidyverse)


spo <- read_csv("spo_allreg_stats.csv")

spo %>% #colnames()
  filter(region == "Волгоградская область" & year == 2018) %>% 
  #filter(field == 2) %>% 
  mutate(accepted = accepted_budget_total + accepted_commerc) %>% 
  pull(accepted) %>% sum()
  

spo_comb <- read_csv("SPO_all_combined.csv")

spo_comb %>% 
  filter(region == "Ростовская область" & 
           year == 2016 &
           type == "Государственные" & 
           form == "очная") %>% 
  summarise(app_b = sum(applied_budget),
            app_c = sum(applied_commerc),
            acc_t = sum(accepted_total))

spo_comb %>% 
  filter(region == "Ростовская область" & 
           year == 2017 &
           type == "Государственные" & 
           form == "очная") %>% 
  summarise(app_b = sum(applied_budget),
            app_c = sum(applied_commerc),
            acc_t = sum(accepted_total))

spo %>% 
  filter(
    region == "Ростовская область" & 
      year == 2016 &
      type == "Государственные" & 
      form == "очная") %>% 
  summarise(app_b = sum(applied_budget),
            app_c = sum(applied_commerc),
            acc_t = sum(accepted_budget_total, accepted_commerc))
