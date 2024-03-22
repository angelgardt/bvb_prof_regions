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


