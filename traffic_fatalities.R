library('readr')
library('haven')
library('dplyr')
library('tidyr')
library('stringr')
library('ggplot2')

#load data
acc2014 <- read_sas("~/Desktop/Repositories/dataviz/accident.sas7bdat")
acc2015 <- read_csv("~/Desktop/Repositories/dataviz/accident.csv")

#convert empty strings to NA in 2014 data
acc2014 <- mutate(acc2014, TWAY_ID2 = na_if(TWAY_ID2, ""))

#identify missing columns
colnames(acc2014) %in% colnames(acc2015) #column 19 not in 2015
colnames(acc2015) %in% colnames(acc2014) #columns 19-21 not in 2014
acc2014[19] #ROAD_FNC not in acc2015
acc2015[19:21] #RUR_URB, FUNC_SYS, RD_OWNER not in acc2014

#combine dataframes
acc <- bind_rows(acc2014, acc2015)

#frequency table for RUR_URB
count(acc, vars=RUR_URB) # ~30k NA values because this column was only present in one of the two dataframes
