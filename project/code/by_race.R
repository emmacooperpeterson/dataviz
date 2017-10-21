library("tidyverse")
library(extrafont)
loadfonts()

setwd('~/Desktop/Repositories/dataviz/project/data/')

#################
### LOAD DATA ###
#################

cases <- read_csv("htd/cases.csv")
crime_locations <- read_csv("htd/crime_locations.csv")
defendants <- read_csv("htd/defendants.csv")
judges <- read_csv("htd/judges.csv")

################
### CLEANING ###
################

#remove unwanted columns (rewrite to avoid repetition)
judges <- judges[c("id", "gender", "race", "appointed_by")]
defendants <- defendants[c("case_id", "judge_id", "gender", "race", "total_sentence")]
cases <- cases[c("case_id", "adult_sex", "minor_sex", "labor", "number_victims_male", "number_victims_female")]
crime_locations <- crime_locations[c("case_id", "state")]

#create factors
defendants$gender <- factor(defendants$gender)
levels(defendants$gender) <- c("male", "female", "unknown")

defendants$race <- factor(defendants$race)
levels(defendants$race) <- c("white", "black", "hispanic", "asian", "indian", "other")

judges$appointed_by <- factor(judges$appointed_by)

#######################
### CREATE VARIABES ###
#######################

#u.s. census regions
northeast <- c("Connecticut", "CT", "Maine", "ME", "Massachusetts", "MA", 
               "New Hampshire", "NH", "Rhode Island", "RI", "Vermont", 
               "VT", "New Jersey", "NJ", "New York", "NY", "Pennsylvania", "PA", "US-Canada Border",
               "John F. Kennedy International Airport", "John F Kennedy Airport",
               "John F Kennedy International Airport", "Newark Airport", 
               "Philadelphia International Airport")

midwest <- c("Illinois", "IL", "Indiana", "IN", "Michigan", "MI", "Ohio", "OH",
             "Wisconsin", "WI", "Iowa", "IA", "Kansas", "KS", "Minnesota", 
             "MN", "Missouri", "MO", "Nebraska", "NE", "ND", "SD", 
             "North Dakota", "South Dakota", "John Glenn Columbus International Airport", 
             "Kansas City International Airport", "Minneapolis-St. Paul Airport", 
             "O'Hare International Airport")

south <- c("Delaware", "DE", "Florida", "FL", "Georgia", "GA", "Maryland", "MD",
           "North Carolina", "NC", "South Carolina", "SC", "Virginia", "VA", 
           "District of Columbia", "DC", "West Virginia", "WV", "Alabama", 
           "AL", "Kentucky", "KY", "Mississippi", "MS", "Tennessee", "TN",
           "Arkansas", "AR", "Louisiana", "LA", "Oklahoma", "OK", "Texas", "TX",
           "Dulles International Airport", "US-Mexico Border", 
           "United States-Mexico Border", "DFW Airport", "Rio Grande River")

west <- c("Arizona", "AZ", "Colorado", "CO", "Idaho", "ID", "Montana", "MT", 
          "Nevada", "NV", "New Mexico", "NM", "Utah", "UT", "Wyoming", "WY", 
          "Alaska", "AK", "California", "CA", "Hawaii", "HI", "Oregon", 
          "OR", "Washington", "WA", "San Francisco International Airport", 
          "Antonio B. Won Pat International Airport", "King County", 
          "Los Angeles Cruise Ship Docks", "Los Angeles International Airport",
          "Oakland International Airport", "Oroville, Okanogan County",
          "Pago Pago International Airport", "Sumas County", "Whatcom County")

#region of crime location
crime_locations$region <- NA
crime_locations$region[crime_locations$state %in% south] <- "south"
crime_locations$region[crime_locations$state %in% northeast] <- "northeast"
crime_locations$region[crime_locations$state %in% west] <- "west"
crime_locations$region[crime_locations$state %in% midwest] <- "midwest"
crime_locations$region <- factor(crime_locations$region)
crime_locations$region <- factor(crime_locations$region, 
                                 levels(crime_locations$region)[c(1,2,4,3)])
crime_locations <- crime_locations[c("case_id", "region")]


#type of trafficking
cases$trafficking_type <- NA
cases$trafficking_type[cases$adult_sex == "true"] <- "sex"
cases$trafficking_type[cases$minor_sex == "true"] <- "minor sex"
cases$trafficking_type[cases$labor == "true"] <- "labor"
cases$trafficking_type <- factor(cases$trafficking_type)
cases$trafficking_type <- factor(cases$trafficking_type, 
                                 levels(cases$trafficking_type)[c(1,3,2)])

#victim gender (majority)
cases$victim_gender <- NA
cases$victim_gender[cases$number_victims_female > cases$number_victims_male] <- "female"
cases$victim_gender[cases$number_victims_male > cases$number_victims_female] <- "male"
cases <- cases[c("case_id", "trafficking_type", "victim_gender")]

#judge variables
judges$judge_white <- NA
judges$judge_white[judges$race == "White"] <- "white"
judges$judge_white[judges$race != "White"] <- "non-white"

#defendant
defendants <- filter(defendants, total_sentence < 999)
defendants$total_sentence <- defendants$total_sentence / 12

#rename
colnames(judges) <- c("judge_id", "judge_gender", "judge_race", "appointed_by", "judge_white")
colnames(defendants) <- c("case_id", "judge_id", "defendant_gender", "defendant_race", "total_sentence")


judges$judge_gender <- factor(judges$judge_gender)
judges$judge_race <- factor(judges$judge_race)
judges$judge_white <- factor(judges$judge_white)
cases$victim_gender <- factor(cases$victim_gender)

#JOIN ALL
traf <- inner_join(defendants, judges, by='judge_id') %>%
  inner_join(., cases, by='case_id')  %>%
  inner_join(., crime_locations, by="case_id")

max <- mean(traf$total_sentence) + 2 * sd(traf$total_sentence)

traf <- filter(traf, total_sentence < max)

traf <- filter(traf, !is.na(defendant_gender) & !is.na(defendant_race) 
               & !is.na(total_sentence) & !is.na(judge_gender)
               & !is.na(judge_race) & !is.na(appointed_by)
               & !is.na(judge_white) & !is.na(trafficking_type)
               & !is.na(victim_gender) & !is.na(region))


############
### PLOT ###
############

my_theme <- theme(plot.background = element_rect(fill="#F4F4F4"),
                  plot.margin=unit(c(1,1,1,1),"cm"),
                  plot.caption = element_text(family="Montserrat Light", size=6,
                                              margin=margin(t=20)),
                  plot.title = element_text(family="Montserrat", face="bold", size=15),
                  plot.subtitle = element_text(family="Courier New"),
                  
                  panel.background = element_rect(fill = "#F4F4F4"),
                  panel.grid.major.y = element_line(color="black", size=0.25),
                  panel.grid.minor.y = element_line(color="black", size=0.25),
                  panel.grid.minor.x=element_blank(),
                  panel.grid.major.x=element_blank(),
                  
                  legend.background = element_rect(fill="#F4F4F4"),
                  legend.key = element_blank(),
                  legend.title = element_text(family="Montserrat", size=10),
                  legend.text = element_text(family="Courier New", size=8),
                  
                  axis.ticks.y = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text = element_text(family="Montserrat Light", size=7),
                  axis.text.x = element_text(angle = 45, hjust = 1, margin=margin(t=-10)),
                  axis.title = element_text(family="Montserrat", size=8),
                  axis.title.x = element_text(margin=margin(t=15)),
                  axis.title.y = element_text(margin=margin(r=15)))


ggplot(filter(traf, defendant_race != "indian" & defendant_race != "other")) +
  
  geom_boxplot(aes(x=judge_race, y=total_sentence, fill=judge_race), 
               width=0.75, outlier.shape=NA) +
  
  geom_boxplot(aes(x=defendant_race, y=total_sentence, fill=defendant_race), 
               width=0.75, outlier.shape=NA) +
  
  labs(y = "Length of Prison Sentence (years)", 
       title=paste("In human trafficking cases, Black defendants", 
                   "\nreceive the longest sentences while ",
                   "Asian \njudges deliver the longest sentences", sep=""),
       subtitle="Varying prison stays for perpetrators of human trafficking",
       caption="Source: www.HumanTraffickingData.org") +
  
  scale_fill_manual(name="Judge Race",
                    values=c("#262c33", "#546373", "#b3920a", "#f2c60e", 
                             "#7f807a", "#bebfb6", "#bf7711", "#ff9f17"),
                    labels=c("Asian Defendant", "Asian Judge", "Black Defendant",
                             "Black Judge", "Hispanic Defendant", "Hispanic Judge",
                             "White Defendant", "White Judge")) +
  
  my_theme +
  
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  
  scale_y_continuous(limits=c(0, 25)) +
  
  guides(fill=guide_legend(keywidth=0.5, keyheight=1, default.unit="cm")) +
  
  scale_x_discrete(labels=c("Asian \ndefendant", "Asian \njudge", 
                            "Black \ndefendant", "Black \njudge",
                            "Hispanic \ndefendant", "Hispanic \njudge",
                            "White \ndefendant", "White \njudge"))