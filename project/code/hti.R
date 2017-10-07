library(tidyverse)

setwd('~/Desktop/Repositories/dataviz/project/data/')

#read data
hti <- read_dta("hti/hti_data.dta")

variables <- c("country", "ccode", "year", "tier", "minstand", "efforts",
               "domesticlaws", "enforcement", "convictinfo", "prosnum",
               "protectprogress", "victimid", "victimservices", "victimpunish",
               "preventprogress", "UNP_sign", "UNP_rat", "UNCRC_sign", 
               "UNCRC_rat", "conflict_sign", "conflict_rat", "CEDAW_sign",
               "CEDAW_rat", "ILO29", "ILO105", "ILO182")

hti <- hti[variables]

#number of countries offering victim services by year
victim_services <- hti %>% group_by(victimservices, year) %>% tally()

#add year_totals and calculate percentage
yt <- summarise(group_by(victim_services, year), year_total = sum(n))
victim_services <- inner_join(victim_services, yt)
victim_services$percentage <- victim_services$n / victim_services$year_total

#convert numbers to no/unknown/yes
victim_services$victimservices <- as.integer(victim_services$victimservices)
victim_services$victimservices[victim_services$victimservices == -1] <- "no"
victim_services$victimservices[victim_services$victimservices == 0] <- "unknown"
victim_services$victimservices[victim_services$victimservices == 1] <- "yes"

#ignore unknown
victim_services <- subset(victim_services, victimservices != "unknown" )

#plot
ggplot(victim_services, aes(x=year, y=percentage, color=victimservices)) +
  
  geom_line() +
  
  labs(y = "Number of Countries Providing Victim Services", x = "Year", 
       title="Services for Victims of Human Trafficking",
       subtitle="Majority of countries fail to provide services for victims",
       caption="Source: Human Trafficking Indicators, 2000-2011 (Harvard Dataverse)") +
  
  scale_color_manual(name="Victim\nServices",
                       values=c("#303841", "#F6C90E"), 
                       labels=c("Not Offered", "Offered")) +

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

  scale_x_continuous(breaks=seq(2001,2011),
                     labels=seq(2001,2011)) +

  scale_y_continuous(breaks=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
                     labels=c("10%", "20%", "30%", "40%", "50%", "60%"))


