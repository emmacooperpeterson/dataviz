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

#read fips data
fips <- read_csv("~/Desktop/Repositories/dataviz/fips.csv")

#convert acc state & county columns to character
acc$STATE <- as.character(acc$STATE)
acc$COUNTY <- as.character(acc$COUNTY)

#add leading zeros
acc$STATE <- str_pad(acc$STATE, 2, side=c("left"), pad="0")
acc$COUNTY <- str_pad(acc$COUNTY, 3, side=c("left"), pad="0")

#rename state & county columns
acc <- rename(acc, "StateFIPSCode" = STATE, "CountyFIPSCode" = COUNTY)

#combine acc and fips
acc <- left_join(acc, fips, by = c("StateFIPSCode", "CountyFIPSCode"))

#count fatalities by state
fatalities <- group_by(acc, StateName, YEAR)
agg <- summarise(fatalities, TOTAL = sum(FATALS))

#convert to wide form and rename columns
agg_wide <- spread(agg, YEAR, TOTAL)
agg_wide <- rename(agg_wide, "year2014" = "2014", "year2015" = "2015")

#calculate percent difference
agg_wide <- mutate(agg_wide, percent_diff = (year2015 - year2014) / year2014)
agg_wide <- arrange(agg_wide, desc(agg_wide$percent_diff))

#capture states with > 15% change and remove NA state
agg_wide <- filter(agg_wide, percent_diff > 0.15 & !is.na(StateName))

#prior six steps using one assignment
agg <- acc %>%
  group_by(StateName, YEAR) %>%
  summarise(TOTAL = sum(FATALS)) %>%
  spread(YEAR, TOTAL) %>%
  rename("year2014" = "2014", "year2015" = "2015") %>%
  mutate(percent_diff = (year2015 - year2014) / year2014) %>%
  arrange(desc(percent_diff)) %>%
  filter(percent_diff > 0.15 & !is.na(StateName))

glimpse(agg)
