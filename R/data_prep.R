

library(tidyverse)
library(readxl)
library(qxl)

update_relevant <- function(x, swap) {
  for (j in seq_along(swap)) {
    x <- gsub(
      paste0("(?<=\\{)", as.character(swap[j]), "(?=\\})"),
      names(swap[j]),
      x,
      perl = TRUE
    )
  }
  
  x
}


## recreate mortality survey data and kobo dict, but with shorter variable names
dict_survey <- readxl::read_xls("data-raw/KoboMortalitySurvey.xls", sheet = 1)
dict_choices <- readxl::read_xls("data-raw/KoboMortalitySurvey.xls", sheet = 2)
dict_settings <- readxl::read_xls("data-raw/KoboMortalitySurvey.xls", sheet = 3)

names_shorten <- setNames(
  dict_survey$name[!is.na(dict_survey$name)],
  dict_survey$name_short[!is.na(dict_survey$name)]
)

dict_survey_out <- dict_survey %>% 
  select(-name) %>% 
  rename(name = name_short) %>% 
  mutate(relevant = map_chr(relevant, update_relevant, swap = names_shorten))

dat_hh <- readxl::read_xlsx("data-raw/MortalitySurveyData.xlsx", sheet = 1)
dat_mb <- readxl::read_xlsx("data-raw/MortalitySurveyData.xlsx", sheet = 2)

dat_hh_short <- dat_hh %>% rename(!!!any_of(names_shorten))
dat_mb_short <- dat_mb %>% rename(!!!any_of(names_shorten))

qxl::qxl(
  list(`Mortality Survey` = dat_hh_short, hh_member = dat_mb_short),
  file = "data/mortality_survey_data.xlsx"
)

qxl::qxl(
  list(survey = dict_survey_out, choices = dict_choices, settings = dict_settings),
  file = "data/mortality_survey_kobo.xlsx"
)

rm(dict_survey, dict_choices, dat_hh, dat_mb)




## create a simpler version of mortality survey dataset by merging a few cols of
# household-level data with member-level data

dict_survey <- readxl::read_xlsx("data/mortality_survey_kobo.xlsx", sheet = 1)
dict_choices <- readxl::read_xlsx("data/mortality_survey_kobo.xlsx", sheet = 2)
dict_settings <- readxl::read_xlsx("data/mortality_survey_kobo.xlsx", sheet = 3)

dat_hh <- readxl::read_xlsx("data/mortality_survey_data.xlsx", sheet = 1)
dat_mb <- readxl::read_xlsx("data/mortality_survey_data.xlsx", sheet = 2)

dat_simple_prep <- dat_mb %>% 
  select(
    id = `_index`,
    `_parent_index`,
    sex:cause_death_other
  ) %>% 
  left_join(dat_hh, by = c("_parent_index" = "_index")) %>% 
  select(
    id,
    date,
    location,
    cluster,
    source_water,
    source_water_other,
    sex:cause_death_other
  )

set.seed(59402910)

dat_simple <- dat_simple_prep %>% 
  slice_sample(n = 1000) %>% 
  mutate(id = paste0("PID", stringr::str_pad(1:n(), width = 3, pad = "0"))) %>% 
  mutate(cluster = as.character(sample(1:5, n(), replace = TRUE)))

dat_simple %>% 
  count(location, cluster)

dat_hh %>% 
  count(source_water, source_water_other)

dat_simple %>% 
  count(source_water, source_water_other)

dat_simple %>% 
  count(arrived, departed, born, died)

dat_simple %>% 
  count(died, cause_death, cause_death_other)


qxl::qxl(
  list(`Mortality Survey` = dat_simple),
  "data/mortality_survey_simple_data.xlsx"
)

dict_survey_simple_prep <- dict_survey %>% 
  filter(name %in% names(dat_simple)) %>% 
  mutate(list_name = stringr::str_extract(type, pattern = "(?<= )\\w*$"), .after = type)

dict_choices_simple <- dict_choices %>% 
  semi_join(dict_survey_simple_prep, by = "list_name")

dict_survey_simple <- dict_survey_simple_prep %>% 
  select(-list_name)

qxl::qxl(
  list(survey = dict_survey_simple, options = dict_choices_simple, settings = dict_settings),
  "data/mortality_survey_simple_kobo.xlsx"
)
