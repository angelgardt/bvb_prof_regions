library(tidyverse)

# spo <- read_csv("SPO_all_combined.csv")

spo <- read_csv("spo_allreg_stats.csv")

spo %>% #colnames()
  filter(region == "Волгоградская область" & year == 2018) %>% 
  #filter(field == 2) %>% 
  mutate(accepted = accepted_budget_total + accepted_commerc) %>% 
  pull(accepted) %>% sum()
  
