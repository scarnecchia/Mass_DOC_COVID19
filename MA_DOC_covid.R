rm(list=ls()) 
library(fs)
setwd(path("D:", "Github", "MDOC_COVID"))

# Lookup tables and globals -------------------------------------------------------------
prison_names = read_csv(path("inputfiles", "inst_name_lkp.csv"))
data_url <- "https://docs.google.com/spreadsheets/d/1nmZ84rjOxQgdTL0PdV7SrbyDTbD7nROQ/export#gid=1419540291"

# Source Packages and Functions ---------------------------------------------------------
source(path("inputfiles","common.R"))
source(path("inputfiles", "functions.R"))


# Data ----------------------------------------------------------------------------------
tf <- fetch_doc_data(data_url)
cumulative_cases = mass_doc_cumulative(tf)
data <- prison_names %>% 
  pull(fac) %>% 
  unique() %>% 
  map(mass_doc_inst_month)
  
names(data) <- prison_names %>% 
  pull(fac) %>% 
  unique()

imap(data, pandoc.table.return, style="grid") %>% write_lines("markdown.md")
# Cases Per Institution, by Month --------------------------------------------------------------------------------------------------

