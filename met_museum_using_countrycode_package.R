library(tidyverse)
library(countrycode)
library(readr)
MetObjects <- read_csv("../MetObjects.csv", locale = locale())
x<-countrycode::codelist

met_by_country<-MetObjects %>% 
  janitor::clean_names() %>% 
  filter(!is.na(country)) %>% 
  mutate(country=gsub("probably |possibly | *\\(.*?\\) *|\\?", "", country)) %>% 
  mutate(country=map(str_split(string=country, pattern="/|,|\ or\ |\\|"), `[[`,1) %>% unlist()) %>% 
  mutate(country=case_when(
    country %in% c("England","Scotland","Wales") ~"United Kingdom",
    grepl("Briti", country) ~ "United Kingdom",
    country=="America" ~ "United States",
    TRUE ~ country
  )) %>% 
  mutate(iso=countrycode(country, 'country.name', 'iso3c')) %>% 
  filter(!is.na(iso))

saveRDS(met_by_country, "met_country.rds")
