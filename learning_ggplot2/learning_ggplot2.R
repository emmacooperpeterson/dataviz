# http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html

# Building blocks of a graph include:

# data
# aesthetic mapping
# geometric object
# statistical transformations
# scales
# coordinate system
# position adjustments
# faceting

# set working directory
setwd("~/Desktop/Repositories/dataviz/learning_ggplot2/")

#look at housing prices
housing <- read.csv("Rgraphics/dataSets/landdata-states.csv")
head(housing[1:5])

#subset by date
hp2001Q1 <- subset(housing, Date == 2001.25) 

#scatterplot
ggplot(hp2001Q1,
      aes(y = Structure.Cost, x = Land.Value)) +
      geom_point()

#same plot w log scale
ggplot(hp2001Q1,
      aes(y = Structure.Cost, x = log(Land.Value))) +
      geom_point()

#plot w regression line

hp2001Q1$pred.SC <- predict(lm(Structure.Cost ~ log(Land.Value), data = hp2001Q1))

p1 <- ggplot(hp2001Q1, aes(x = log(Land.Value), y = Structure.Cost)) +
        geom_point(aes(color = Home.Value)) +
        geom_line(aes(y = pred.SC))

#smoothing
p1 +
  geom_point(aes(color = Home.Value)) +
  geom_smooth()

#text labels
p1 + 
  geom_text(aes(label=State), size = 3)

#better text labels
library("ggrepel")
p1 + 
  geom_point() + 
  geom_text_repel(aes(label=State), size = 3)

#shapes
p1 +
  geom_point(aes(color=Home.Value, shape = region))

##################
### EXERCISE 1 ###
##################

dat <- read.csv("Rgraphics/dataSets/EconomistData.csv")
head(dat)

#Create a scatter plot with CPI on the x axis and HDI on the y axis.
#Color the points blue.
ggplot(dat, aes(x=CPI, y=HDI)) + geom_point(color="blue")

#Map the color of the the points to Region.
#Make the points bigger by setting size to 2
ggplot(dat, aes(x=CPI, y=HDI, color=Region)) + geom_point(size=2)

#Map the size of the points to HDI.Rank
ggplot(dat, aes(x=CPI, y=HDI, size=HDI.Rank, color=Region)) + geom_point()

###################################
### STATISTICAL TRANSFORMATIONS ###
###################################

p2 <- ggplot(housing, aes(x = Home.Value))
p2 + geom_histogram(stat="bin", binwidth=4000)

##################
### EXERCISE 2 ###
##################

# Re-create a scatter plot with CPI on the x axis and HDI on the y axis
p3 <- ggplot(dat, aes(x=CPI, y=HDI)) + geom_point(color="blue")

# Overlay a smoothing line on top of the scatter plot using geom_smooth.
p3 + geom_smooth()

# Overlay a smoothing line on top of the scatter plot using geom_smooth, 
# but use a linear model for the predictions. Hint: see ?stat_smooth.
p3 + stat_smooth()

# Overlay a smoothing line on top of the scatter plot using geom_line. 
#Hint: change the statistical transformation.
p3 + geom_line(stat="identity")

# BONUS: Overlay a smoothing line on top of the scatter plot using the default loess method, 
# but make it less smooth. Hint: see ?loess.
p3 + geom_smooth(span=0.2)

##########################
### SCALE MODIFICATION ###
##########################

#Start by constructing a dotplot showing the distribution of home values by Date and State.

p4 <- ggplot(housing,
             aes(x = State,
                 y = Home.Price.Index)) + 
      theme(legend.position="top",
            axis.text=element_text(size = 6))
(p5 <- p4 + geom_point(aes(color = Date),
                       alpha = 0.5,
                       size = 1.5,
                       position = position_jitter(width = 0.25, height = 0)))

#Now modify the breaks for the x axis and color scales

p5 + scale_x_discrete(name="State Abbreviation") +
  scale_color_continuous(name="",
                         breaks=c(1976,1994,2013),
                         labels=c("'76","'94","'13"))


#Next change the low and high values to blue and red:
  
p5 +
  scale_x_discrete(name="State Abbreviation") +
  scale_color_continuous(name="",
                         breaks = c(1976, 1994, 2013),
                         labels = c("'76", "'94", "'13"),
                         low = "blue", high = "red")

#with muted colors
library(scales)
p5 +
  scale_color_continuous(name="",
                         breaks = c(1976, 1994, 2013),
                         labels = c("'76", "'94", "'13"),
                         low = muted("blue"), high = muted("red"))

#use a different color scale
p5 +
  scale_color_gradient2(name="",
                        breaks = c(1976, 1994, 2013),
                        labels = c("'76", "'94", "'13"),
                        low = muted("blue"),
                        high = muted("red"),
                        mid = "gray60",
                        midpoint = 1994)

#type scale_ and hit tab to get full list of available scales

##################
### EXERCISE 3 ###
##################

# Create a scatter plot with CPI on the x axis and HDI on the y axis. 
#Color the points to indicate region.
p6 <- ggplot(dat, aes(x=CPI, y=HDI, color=Region)) + geom_point()

# Modify the x, y, and color scales so that they have more easily-understood names 
# (e.g., spell out “Human development Index” instead of “HDI”).
#Modify the color scale to use specific values of your choosing. 
p6 + scale_color_manual(values=c("red", "orange", "yellow", "green", "blue", "pink"), 
                        labels=c("Americas", "Asia Pacific", "East EU + Central Asia", 
                                 "Western European EU", "Middle East + North Africa", 
                                 "Sub-Saharan Africa")) +
  labs(y = "Human Development Index", x = "Corruption Perception Index")

################
### FACETING ###
################

#What is the trend in housing prices in each state?
#Start by using a technique we already know–map State to color:

p7 <- ggplot(housing, aes(x = Date, y = Home.Value))
#p7 + geom_line(aes(color = State))

# too many lines/colors to be useful! facet instead
(p7 <- p7 + geom_line() +
    facet_wrap(~State, ncol = 10))

##############
### THEMES ###
##############

p7 + theme_linedraw()
p7 + theme_light()

#override theme defaults
p7 + theme_minimal() +
  theme(text = element_text(color="turquoise"))

#creating and saving new themes
theme_new <- theme_bw() +
  theme(plot.background = element_rect(size = 1, color = "blue", fill = "black"),
        text=element_text(size = 12, family = "Serif", color = "ivory"),
        axis.text.y = element_text(colour = "purple"),
        axis.text.x = element_text(colour = "red"),
        panel.background = element_rect(fill = "pink"),
        strip.background = element_rect(fill = muted("orange")))

p7 + theme_new

#Challenge: Recreate This Economist Graph
#http://www.economist.com/node/21541178

to_label <- c("Congo", "Afghanistan", "Sudan", "Myanmar", "Iraq", "Rwanda",
              "India", "South Africa", "China", "Venezuela", "Russia", "Argentina",
              "Greece", "Brazil", "Italy", "Bhutan", "Cape Verde", "Botswana",
              "Spain", "France", "US", "Germany", "Britain", "Barbados", "Japan",
              "Norway", "Singapore", "New Zealand")

p8 <- ggplot(dat, aes(x=CPI, y=HDI)) + geom_point(size=3, aes(color=Region), pch=21, stroke=1)

p8 + scale_color_manual(values=c("turquoise3", "cyan", "turquoise4", "dodgerblue4", "firebrick2", "firebrick4"), 
                        labels=c("Americas", "Asia & Oceania", "Central & Eastern Europe", 
                                 "OECD", "Middle East & north Africa", 
                                 "Sub-Saharan Africa")) +
  
  labs(y = "Human Development Index, 2011 (1=best)", x = "Corruption Perception Index, 2011 (10=least corrupt)", 
       title="Corruption and human development") +
  
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10),
    labels=c(1,2,3,4,5,6,7,8,9,10)) +
  
  scale_y_continuous(breaks=c(.2,.3,.4,.5,.6,.7,.8,.9,1),
                   labels=c(.2,.3,.4,.5,.6,.7,.8,.9,1)) +
  
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        plot.title = element_text(face="bold"),
        axis.title = element_text(face="italic", size=6),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color="gray", size=0.25),
        axis.ticks.y = element_line(size=0)) +
  
  geom_smooth(se=FALSE, color="red", span=0.95) +
  
  geom_text_repel(data=filter(dat, Country %in% to_label), aes(label=Country), 
                  size=3, point.padding=0.2, max.iter=5000)
