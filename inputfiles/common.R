list_packages = c("haven", "tidyverse", "tibble", "ggthemes", "scales", "lubridate", "openxlsx", "bookdown", "janitor","gridExtra","formatR", "tufte")
new_packages <- list_packages[!(list_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(readxl)
library(tibble)
library(lubridate)
library(ggthemes)
library(scales)
library(janitor)
library(bookdown)
library(openxlsx)
# library(haven)
library(formatR)
library(tufte)
library(gridExtra)
library(httr)
library(tidyverse)
library(zoo)
library(pander)

panderOptions("table.split.table", Inf) 

current_date = today()