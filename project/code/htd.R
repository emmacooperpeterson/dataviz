library("tidyverse")

setwd('~/Desktop/Repositories/dataviz/project/data/')

cases <- read_csv("htd/cases.csv")
base_locations <- read_csv("htd/base_locations.csv")
crime_locations <- read_csv("htd/crime_locations.csv")
criminal_methods <- read_csv("htd/criminal_methods.csv")
defendants <- read_csv("htd/defendants.csv")
entry_ports <- read_csv("htd/entry_ports.csv")
judges <- read_csv("htd/judges.csv")
victim_countries <- read_csv("htd/victim_countries.csv")

#subset (rewrite to avoid repetition)
drop <- c("created_at", "updated_at", "id")

entry_ports <- entry_ports[ , !(names(entry_ports) %in% drop)]
victim_countries <- victim_countries[, !(names(victim_countries) %in% drop)]
criminal_methods <- criminal_methods[, !(names(criminal_methods) %in% drop)]
crime_locations <- crime_locations[, !(names(crime_locations) %in% drop)]
base_locations <- base_locations[, !(names(base_locations) %in% drop)]

cases_drop <- c("created_at", "updated_at", "id", "assigned_user_id", "user_id",
                "dropbox_url", "case_number", "case_name", "case_status")

cases <- cases[, !(names(cases) %in% cases_drop)]

defendants <- defendants[c("case_id", "judge_id", "number_of_defendants",
                           "first_name", "last_name", "alias", "gender", "race",
                           "country_of_origin", "birth_year", "arrest_age",
                           "arrest_date", "detained", "bail_type", "bail_amount",
                           "felonies_charged", "felonies_sentenced", "date_terminated",
                           "sentenced_date", "total_sentence", "restitution",
                           "charged_with_forfeiture", "sentenced_with_forfeiture",
                           "appeal", "sup_release", "probation")]



#variable creation
judges$years_in_office <- 2017 - judges$tenure

#############
### PLOTS ###
#############

ggplot(subset(judges, !is.na(judges$gender)), aes(x=years_in_office, y=case_count, color=gender)) +
  geom_point()

ggplot(judges, aes(x=gender, y=case_count)) +
  geom_col()


