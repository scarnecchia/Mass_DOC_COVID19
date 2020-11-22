# Packages ------------------------------------------------------------------------------

list_packages = c("haven", "tidyverse", "tibble", "ggthemes", "scales", "lubridate", "openxlsx", "bookdown", "janitor","gridExtra","formatR", "tufte","httr","rmdwiki","zoo","readxl")
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
library(rmdwiki)
library(tidyverse)
library(zoo)
#library(pander)
#library(reprex)

# Global Values -------------------------------------------------------------------------

current_date = today()
opts <- options(knitr.kable.NA = "**")