# Functions -----------------------------------------------------------------------------
# Load Custom Functions

# Fetch and Clean DOC data --------------------------------------------------------------
fetch_doc_data <- function(data_url) {
  if(file_exists(Sys.glob(path_wd("inputfiles", "file*.xlsx"))) && difftime(Sys.time(), 
                                                                            file_info(Sys.glob(path_wd("inputfiles", "file*.xlsx")))$modification_time, unit = "hours") <= 24) {
    tf <- file_info(Sys.glob(path_wd("inputfiles", "file*.xlsx"))) %>% 
      filter(modification_time == max(modification_time)) %>% 
      pull(path)
  } else {
  GET(data_url, write_disk(tf <- file_temp(tmp_dir = path_wd("inputfiles"), ext = ".xlsx")))
  }
}

clean_doc_data <- function(x) {
  df <- read_excel(x, sheet=2) %>%
    mutate(Date = as.Date(Date)) %>% 
    mutate(across(where(is.character), ~na_if(., 'NA'))) %>% 
    rename(fac = `DOC Facility`,
           total_population = `Total Population`,
           all_positive = `Total Positive`,
           all_tested = `Total Tested`,
           all_released = `N Released`,
           staff_positive = `N Positive - COs`,
           staff_tested = `N Tested - COs`,
           inmates_positive =  `N Positive - Detainees/Inmates`,
           inmates_tested = `N Tested - Detainees/Inmates`,
           deaths = `N Deaths`) %>%
    select(-Notes, -`Active Prisoner Cases`) %>% 
    mutate(across(3:11, ~as.numeric(.))) %>% 
    # mutate(inmates_positive_rate = inmates_positive/inmates_tested, inmates_positive_rate=scales::percent(inmates_positive_rate)) %>% 
    # mutate(staff_positive_rate = staff_positive/staff_tested, staff_positive_rate = scales::percent(staff_positive_rate)) %>% 
    # mutate(inmate_cases_per_100k = (inmates_positive / total_population) * 100000) %>% 
    mutate(fac = factor(fac, levels = unique(fac)))

  return(df)
}

# Calculate cumulative cases ------------------------------------------------------------
mass_doc_cumulative <- function(x) {
df <- clean_doc_data(x) %>%
  select(-total_population, -all_tested, -all_positive, -all_released) %>% 
  filter(!fac=="Non-Facility") %>% 
  group_by(fac) %>% 
  summarise(across(where(is.numeric), sum, na.rm=TRUE)) %>% 
  mutate(inmates_positive_rate = inmates_positive/inmates_tested, inmates_positive_rate=scales::percent(inmates_positive_rate)) %>% 
  mutate(staff_positive_rate = staff_positive/staff_tested, staff_positive_rate = scales::percent(staff_positive_rate)) %>% 
  full_join(prison_names, by="fac") %>% 
  select(name_link, inmates_tested, inmates_positive, inmates_positive_rate, staff_tested, staff_positive, staff_positive_rate, deaths)
return(df)
}

# Get Monthly Cases by Institution ------------------------------------------------------
mass_doc_inst_month <- function(x) {
  total_pop = clean_doc_data(tf) %>% 
    select(Date, fac, total_population) %>% 
    drop_na(total_population) %>% 
    filter(fac==fac) %>% 
    group_by(Date) %>% 
    mutate(Date = floor_date(ymd(Date), 'month')) %>%
    summarise(total_population = last(total_population)) %>% 
    select(total_population, Date) %>% 
    ungroup()
  
  output = clean_doc_data(tf) %>% 
    select(-total_population, -all_released, -all_tested, -all_positive) %>% 
    filter(fac==fac) %>% 
    mutate(Date = floor_date(ymd(Date), 'month')) %>% 
    mutate(across(where(is.numeric), ~replace_na(., 0))) %>% 
    group_by(Date) %>% 
    summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
    full_join(total_pop, by="Date") %>% 
    mutate(inmates_positive_rate = inmates_positive/inmates_tested, inmates_positive_rate=scales::percent(inmates_positive_rate)) %>% 
    mutate(staff_positive_rate = staff_positive/staff_tested, staff_positive_rate = scales::percent(staff_positive_rate)) %>% 
    mutate(cases_per_100k = (inmates_positive / total_population) * 100000) %>% 
    select(Date, total_population, inmates_tested, inmates_positive, cases_per_100k, inmates_positive_rate, staff_tested, 
           staff_positive, staff_positive_rate, deaths) %>% 
    mutate(Date = as.yearmon(Date))
  
  return(output)
}

mass_doc_inst_month_2 <- function() {
  total_pop <- clean_doc_data(tf) %>% 
    select(Date, fac, total_population) %>% 
    drop_na(total_population) %>% 
    group_by(fac, Date) %>% 
    mutate(Date = floor_date(ymd(Date), 'month')) %>%
    summarise(total_population = last(total_population)) %>% 
    select(total_population, Date, fac) %>% 
    ungroup()
  
  data <- clean_doc_data(tf) %>% 
    select(-total_population, -all_released, -all_tested, -all_positive) %>% 
    # filter(fac==fac) %>% 
    mutate(Date = floor_date(ymd(Date), 'month')) %>% 
    mutate(across(where(is.numeric), ~replace_na(., 0))) %>% 
    group_by(fac, Date) %>% 
    summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
    full_join(total_pop, by=c("fac","Date")) %>% 
    mutate(inmates_positive_rate = inmates_positive/inmates_tested, inmates_positive_rate=scales::percent(inmates_positive_rate)) %>% 
    mutate(staff_positive_rate = staff_positive/staff_tested, staff_positive_rate = scales::percent(staff_positive_rate)) %>% 
    mutate(cases_per_100k = (inmates_positive / total_population) * 100000) %>% 
    select(fac, Date, total_population, inmates_tested, inmates_positive, cases_per_100k, inmates_positive_rate, staff_tested, 
           staff_positive, staff_positive_rate, deaths) %>% 
    mutate(Date = as.yearmon(Date)) %>% 
    full_join(prison_names, by="fac") %>% 
    select(fac, name_link, Date, total_population, inmates_tested, inmates_positive, cases_per_100k, inmates_positive_rate, 
           staff_tested, staff_positive, staff_positive_rate, deaths)
}

render_report <- function(fac) {
  template <- path_wd("inputfiles", "prison_report.Rmd")
  out_file <- path_wd("outputfiles", sprintf("COVID_Report_%s_%s", fac, current_date))
  rmarkdown::render(template,
                    output_file = out_file,
                    params = list(fac = unique(fac)))
}