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

####CLEANING####

#remove outliers
defendants <- filter(defendants, arrest_age < 150 & felonies_sentenced < 999
                     & birth_year > 1900)

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

#ENTRY POINT BY REGION
entries$region <- NA

south <- c("US-Mexico Border", "United States-Mexico Border", "DFW Airport", "Rio Grande River")
north <- c("US-Canada Border")
west <- c("San Francisco International Airport", "Antonio B. Won Pat International Airport",
          "King County", "Los Angeles Cruise Ship Docks", "Los Angeles International Airport",
          "Oakland International Airport", "Oroville, Okanogan County",
          "Pago Pago International Airport", "Sumas County", "Whatcom County")
east <- c("John F. Kennedy International Airport", "John F Kennedy Airport", "Dulles International Airport",
          "John F Kennedy International Airport", "Newark Airport", "Philadelphia International Airport")
midwest <- c("John Glenn Columbus International Airport", "Kansas City International Airport",
             "Minneapolis-St. Paul Airport", "O'Hare International Airport")

entries$region[entries$name %in% south] <- "south"
entries$region[entries$name %in% north] <- "north"
entries$region[entries$name %in% west] <- "west"
entries$region[entries$name %in% east] <- "east"
entries$region[entries$name %in% midwest] <- "midwest"

entries_agg <- summarise(group_by(entries, region), total = n())

#international victims - using world bank regions
east_asia_pacific <- c("Korea, Republic of", "Indonesia", "China", "Vietnam",
                       "Taiwan, Province of China", "Micronesia, Federated States of",
                       "Philippines", "Fiji")
europe_cent_asia <- c("Azerbaijan", "Ukraine", "Czech Republic", "Hungary",
                      "Denmark", "Belarus", "Russia", "France", "Russian Federation",
                      "Estonia")
latin_am_carib <- c("Colombia", "Columbia", "Puerto Rico", "Nicaragua", "Peru",
                    "Honduras", "Mexico", "Guatemala", "El Salvador", "Nigeria",
                    "Brazil", "Dominican Republic")
middle_east_n_af <- c("")
north_am <- c("Canada")
south_asia <- c("")
sub_sah_africa <- c("Togo", "Ghana")

victim_countries$origin_region[victim_countries$victimcountry %in% east_asia_pacific] <- "east asia and pacific"
victim_countries$origin_region[victim_countries$victimcountry %in% europe_cent_asia] <- "europe and central asia"
victim_countries$origin_region[victim_countries$victimcountry %in% latin_am_carib] <- "latin america and caribbean"
victim_countries$origin_region[victim_countries$victimcountry %in% north_am] <- "north america"
victim_countries$origin_region[victim_countries$victimcountry %in% sub_sah_africa] <- "sub-saharan africa"

int_vics <- filter(victim_countries, victimcountry != "United States")
origin_entry <- inner_join(int_vics, entries)
origin_entry <- origin_entry[c("victimcountry", "name", "region", "origin_region")]

origin_entry <- filter(entry_ports, !is.na(name) & !is.na(origin_region) & name != "Greyhound bus stop")

#############
### PLOTS ###
#############

ggplot(origin_entry, aes(x=region, fill=origin_region)) +
  
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  
  labs(y = "Percentage of Cases", x = "Point of Entry to the United States", 
       title="International Trafficking Flows \nto the United States",
       subtitle="Majority of U.S. cases involve Latin American & Caribbean \nvictims transported to the U.S. via the southern border",
       caption="Source: www.HumanTraffickingData.org") +
  
  scale_fill_manual(name="Country of Origin",
                     values=c("#303841", "#F6C90E", "#40616d", "#9A9B94", "#EA9215"), 
                     labels=c("East Asia & Pacific", "Europe & Central Asia",
                              "Latin America & Caribbean", "North America",
                              "Sub-Saharan Africa")) +
  
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        plot.background = element_rect(fill="#F4F4F4"),
        legend.key = element_blank(),
        legend.title = element_text(family="Montserrat", size=10),
        legend.text = element_text(family="Courier New", size=8),
        legend.background = element_rect(fill="#F4F4F4"),
        plot.title = element_text(family="Montserrat", face="bold", size=15),
        plot.subtitle = element_text(family="Courier New"),
        panel.background = element_rect(fill = "#F4F4F4"),
        panel.grid.major.y = element_line(color="black", size=0.25),
        panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_line(size=0),
        axis.ticks.x = element_line(size=0),
        axis.text.x = element_text(angle = 45, hjust = 1, margin=margin(t=-2)),
        axis.text = element_text(family="Montserrat Light", size=7),
        axis.title = element_text(family="Montserrat", size=8),
        axis.title.x = element_text(margin=margin(t=15)),
        axis.title.y = element_text(margin=margin(r=15)),
        plot.margin=unit(c(1,1,1,1),"cm"),
        plot.caption = element_text(family="Montserrat Light", size=6, 
                                    margin=margin(t=20))) +
  
  scale_x_discrete(labels=c("East", "Midwest", "North", "South", "West")) +
  scale_y_continuous(labels = percent)


#################
#judges scatterplot

judges_defendants <- inner_join(defendants, judges, by=c("judge_id" = "id"))
jud_def_vic <- inner_join(judges_defendants, cases, by="case_id")

jud_def_vic <- filter(jud_def_vic, total_sentence < 1000)

cases$trafficking_type <- NA

cases$trafficking_type[cases$adult_sex == "true"] <- "sex"
cases$trafficking_type[cases$minor_sex == "true"] <- "minor sex"
cases$trafficking_type[cases$labor == "true"] <- "labor"

ggplot(jud_def_vic, aes(x=trafficking_type, y=total_sentence, fill=trafficking_type)) +
  geom_boxplot() +
  
  labs(y = "Length of Prison Sentence", x = "Type of Trafficking", 
       title="Length of Prison Sentence \nby Trafficking Type",
       subtitle="Perpetrators of sex trafficking of \nminors receive longest sentences",
       caption="Source: www.HumanTraffickingData.org") +
  
  scale_fill_manual(name="Type of Trafficking",
                    values=c("#9A9B94", "#F6C90E", "#40616d"), 
                    labels=c("Labor", "Sex: \nminor victim(s)",
                             "Sex: \nadult victim(s)")) +
  
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        plot.background = element_rect(fill="#F4F4F4"),
        legend.key = element_blank(),
        legend.title = element_text(family="Montserrat", size=10),
        legend.text = element_text(family="Courier New", size=8),
        legend.background = element_rect(fill="#F4F4F4"),
        plot.title = element_text(family="Montserrat", face="bold", size=15),
        plot.subtitle = element_text(family="Courier New"),
        panel.background = element_rect(fill = "#F4F4F4"),
        panel.grid.major.y = element_line(color="black", size=0.25),
        panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_line(size=0),
        axis.ticks.x = element_line(size=0),
        axis.text.y = element_text(family="Montserrat Light", size=7),
        axis.text.x = element_blank(),
        axis.title = element_text(family="Montserrat", size=8),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin=margin(r=15)),
        plot.margin=unit(c(1,1,1,1),"cm"),
        plot.caption = element_text(family="Montserrat Light", size=6)) +
  
  scale_y_continuous(limits=c(-10, 200)) +
  
  guides(fill=guide_legend(keywidth=0.5, keyheight=1, default.unit="cm"))
