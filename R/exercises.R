
library(tidyverse)
library(rio)
library(datadict)


## Load data

# import dataset
dat <- rio::import("data/mortality_survey_simple_data.xlsx", setclass = "tbl")

# import ODK dictionary (note the main dictionary and multiple-choice options are in separate sheets)
odk_survey <- rio::import("data/mortality_survey_simple_kobo.xlsx", sheet = "survey", setclass = "tbl")
odk_choices <- rio::import("data/mortality_survey_simple_kobo.xlsx", sheet = "options", setclass = "tbl")


## Exercise 1
dict_dat <- datadict::dict_from_data(dat)
dict_odk <- datadict::dict_from_odk(odk_survey, odk_choices)


## Exercise 2
dat_reclass <- dat %>% 
  mutate(
    across(starts_with("date"), as.Date),
    across(c(age_months, age_years, muac), as.integer)
  )

dict_dat_reclass <- datadict::dict_from_data(dat_reclass)


## Exercise 3
dict_odk_options <- datadict::coded_options(dict_odk)
dict_dat_options <- datadict::coded_options(dict_dat_reclass)


## Exercise 4
datadict::valid_dict(dict_dat_reclass)

dict_nonvalid <- dict_dat_reclass
dict_nonvalid$variable_name[6] <- "source_water" # duplicate variable name
dict_nonvalid$type[10] <- NA_character_          # missing variable type

datadict::valid_dict(dict_nonvalid)


## Exercise 5
datadict::valid_data(dat_reclass, dict_dat_reclass)

dat_nonvalid <- dat_reclass[,-12]    # remove column present in dict
dat_nonvalid$age_years[5] <- "5yrs"  # non-valid value of numeric variable

datadict::valid_data(dat_nonvalid, dict_dat_reclass)


