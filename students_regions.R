library(tidyverse)

files <- dir("students_regions", full.names = TRUE)
students_regions <- tibble()

for (file in files) {
  readxl::read_xlsx(file, 
                    sheet = "Раздел 1.3", 
                    skip = 19) %>% 
    select(`1`, `3`) %>% 
    slice(c(12, 22)) %>% 
    mutate(region = file) %>% 
    bind_rows(students_regions) -> students_regions
}


students_regions %>%
  mutate(region = region %>% 
           str_remove_all("students_regions/") %>% 
           str_remove_all(" (ГОУ+НОУ) (город+село).xlsx")) %>%
  summarise(sum = sum(`3`),
            .by = region) %>% 
  write_excel_csv("students_regions.csv")
