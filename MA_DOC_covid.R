library(fs)
setwd(path("D:", "Github", "MDOC_COVID"))
source(path("inputfiles","common.R"))

# Data ----------------------------------------------------------------------------------
prison_names = read_csv(path("inputfiles", "inst_name_lkp.csv"))
sjc_googledrive_url <- "https://docs.google.com/spreadsheets/d/1nmZ84rjOxQgdTL0PdV7SrbyDTbD7nROQ/export#gid=1419540291"

GET(sjc_googledrive_url, write_disk(tf <- file_temp(tmp_dir = path("D:", "Github", "MDOC_COVID"), ext = ".xlsx")))

sjc_DOC_df <- read_excel(tf, sheet=2) %>%
  mutate(Date = as.Date(Date)) %>% 
  mutate(across(where(is.character), ~na_if(., 'NA'))) %>% 
  rename(fac = `DOC Facility`,
         total_population = `Total Population`,
         all_positive = `Total Positive`,
         all_tested = `Total Tested`,
         all_released = `N Released`,
         staff_positive = `N Positive - Staff`,
         staff_tested = `N Tested - Staff`,
         inmates_positive =  `N Positive - Detainees/Inmates`,
         inmates_tested = `N Tested - Detainees/Inmates`,
         deaths = `N Deaths`) %>%
  select(-Notes, -`Active Prisoner Cases`, all_released) %>% 
  mutate(across(3:11, ~as.numeric(.))) %>% 
  mutate(fac = factor(fac, levels = unique(fac)))

sjc_DOC_cum <- sjc_DOC_df %>%
  select(-total_population, -all_released, -all_tested, -all_positive) %>% 
  filter(!fac=="Non-Facility") %>% 
  group_by(fac) %>% 
  summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>% 
  mutate(inmates_positive_rate = inmates_positive/inmates_tested, inmates_positive_rate=scales::percent(inmates_positive_rate)) %>% 
  mutate(staff_positive_rate = staff_positive/staff_tested, staff_positive_rate = scales::percent(staff_positive_rate)) %>% 
  full_join(prison_names, by="fac") %>% 
  select(name_link, inmates_tested, inmates_positive, inmates_positive_rate, staff_tested, staff_positive, staff_positive_rate, deaths)
  
# Cases Per Institution, by Month --------------------------------------------------------------------------------------------------
inst_month <- function(df, x){
  total_pop = df %>% 
    select(Date, fac, total_population) %>% 
    filter(fac==x) %>% 
    group_by(Date) %>% 
    mutate(Date = floor_date(ymd(Date), 'month')) %>%
    summarise(total_population = last(total_population)) %>% 
    select(total_population, Date) %>% 
    ungroup()
  
  output = df %>% 
    select(-total_population, -all_released, -all_tested, -all_positive) %>% 
    filter(fac==x) %>% 
    mutate(Date = floor_date(ymd(Date), 'month')) %>% 
    group_by(Date) %>% 
    summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>% 
    full_join(total_pop, by="Date") %>% 
    mutate(inmates_positive_rate = inmates_positive/inmates_tested, inmates_positive_rate=scales::percent(inmates_positive_rate)) %>% 
    mutate(staff_positive_rate = staff_positive/staff_tested, staff_positive_rate = scales::percent(staff_positive_rate)) %>% 
    mutate(cases_per_100k = (inmates_positive / total_population) * 100000) %>% 
    select(Date, total_population, inmates_tested, inmates_positive, cases_per_100k, inmates_positive_rate, staff_tested, 
           staff_positive, staff_positive_rate, deaths) %>% 
    mutate(Date = as.yearmon(Date))
  return(output)
}
