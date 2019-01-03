setwd("~/Documents/Prostate/project proposals/IBD")
options(stringsAsFactors = FALSE)
library(popEpi)  # sir()
library(tidyverse)
library(magrittr)

# CDC United States Cancer Statistics 1999-2015 from 
# https://www.cdc.gov/cancer/uscs/USCS_1999_2015_ASCII.zip via
# https://www.cdc.gov/cancer/uscs/dataviz/download_data.htm
uscs        <- read.csv(file = "USCS_1999_2015_ASCII/BYAGE.TXT", sep = "|")
names(uscs) <- tolower(names(uscs))
uscs %<>% filter(site == "Prostate" & sex == "Male" & year == "2014" &
                   event_type == "Incidence" & race == "All Races" & rate != "~")
   # Use 2014 incidence rates (end of inclusion in Burns et al.)
uscs %<>% type_convert() %>%
  mutate(agestart = as.numeric(substr(age, 1, 2)))

# Input Burns et al., Eur Urol 2018, Table 1 counts
burns <- data.frame(decade = rep(c(30, 40, 50, 60, 70), 2),                         # Start of age category
                    n      = c(630, 2602, 3304, 1978, 792, 70, 288, 366, 221, 47),  # Person counts 
                    cases  = c(29, rep(NA, 4), 30, rep(NA, 4)),                     # 5-yr cumulative incidence of total PCa
                    group  = as.factor(c(rep("Non-IBD", 5), rep("IBD", 5))),
                    freq   = 2)                                                     # Needed to split data into age categories
# Split counts equally into 5-year age categories instead of 10-year age categories to match the standard population
burns %<>% uncount(freq, .id = "duplicate") %>%
  mutate(agestart = if_else(duplicate == 1, decade, decade + 5),
         n_fu     = n * 5 / 2) %>%  # Table 1 gives 5-year cumulative incidence. Put half into each 5-year category
  filter(!(decade == 30 & duplicate == 1))  # Assign "<40" counts to 35-40 age band only (also, no rates for <35 in std. pop)

# SMRs for IBD and non-IBD groups compared to U.S. population
sir(coh.data = burns,       # Exposed: dataset
    coh.obs  = cases,       # Exposed: Case count (note--total cases per "group" listed in one random stratum of age)
    coh.pyrs = n_fu,        # Exposed: Person-time
    ref.data = uscs,        # Standard: dataset
    ref.obs  = count,       # Standard: Case count
    ref.pyrs = population,  # Standard: Person-time (time presumed to be one year)
    print    = group,       # Exposed: IBD vs. non-IBD
    adjust   = agestart)    # Strata, must be same variable in exposed and standard

