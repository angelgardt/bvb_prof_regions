library(tidyverse)
load("vpo_export.RData")

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

vpo_roi_avgs %>% #View()
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "VPO_roi_groups"
  )

vpo_allreg_avgs %>% #View()
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "VPO_allreg_groups"
  )

vpo_allreg_fields %>% 
  googlesheets4::write_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1YuYjpk5ANbsObbSWgz8K90mGIMqvJAN4nPRUGCdJbHM/edit#gid=0",
    sheet = "VPO_allreg_fields"
  )