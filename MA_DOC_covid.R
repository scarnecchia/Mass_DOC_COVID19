rm(list=ls()) 
library(fs)
setwd(path("D:", "Github", "MDOC_COVID"))

# Source Packages and Functions ---------------------------------------------------------
source(path("inputfiles","common.R"))
source(path("inputfiles", "functions.R"))

# Lookup tables and globals -------------------------------------------------------------
prison_names = read_csv(path("inputfiles", "inst_name_lkp.csv"))
data_url <- "https://docs.google.com/spreadsheets/d/1nmZ84rjOxQgdTL0PdV7SrbyDTbD7nROQ/export#gid=1419540291"
tf = fetch_doc_data(data_url)
df = clean_doc_data(tf)

# Cases Total
cumulative_cases = mass_doc_cumulative(tf)

# Cases Per Institution, by Month --------------------------------------------------------------------------------------------------
param_infos <- tibble(fac = unique(prison_names$fac))

param_infos %>%
  transpose() %>% 
  walk(render_report)