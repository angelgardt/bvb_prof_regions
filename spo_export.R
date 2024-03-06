library(tidyverse)
load("spo_export.RData")

spo_roi_stats %>% #View()
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "SPO_roi_specs"
  )

spo_all_regions_stats %>% #View()
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "SPO_allreg_specs"
  )

spo_roi_avgs %>% 
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "SPO_roi_groups"
  )

spo_allreg_avgs %>% 
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "SPO_allreg_groups"
  )

spo_allreg_fields %>% 
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "SPO_allreg_fields"
  )

