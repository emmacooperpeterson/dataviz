#INTRO TO R - data structures

# atomic vector (one-dimensional, homogenous data structure)
vec <- c(1,2,3,4)

#check type
class(vec) #class of object
typeof(vec) #R's internal type
str(vec) #structure

#copy vec
vec2 <- vec
vec <- c("a", "b", "c") #vec2 still says 1234 - it is not linked to vec

#indexing (INDEXED TO 1 NOT 0)
vec[1] #a

#matrices (two-dimensional, homogenous data structure)
mat <- matrix(c(1,2,3,4), 2, 2) #data, numrows, numcols
mat[1,1] #row 1, col 1
mat[1,2] #row 1, col 2

#lists (one-dimensional, heterogenous data structure)
test <- list(c(1,2), "cat", mtcars)
test[[1]] #double brackets for indexing lists

#ls() - show all the objects currently in memory
#rm(name_of_thing, name_of_thing_2, etc.) to remove things from memory

#getwd() get working directory
setwd("./Desktop/Repositories/dataviz/Class Examples") #use forward slashes for file directories
#dir() shows what's in your working directory

#load data
drugs <- read.csv("NSDUH.csv")
head(drugs) #show first 6 rows
head(drugs, 3) #show first 3 rows  

hist(drugs$AGE2) #$ means within (AGE2 is the column name within drugs dataframe)
#?hist view documentation for a function with ?

#if you put arguments in a different order than the default, you need to name them
hist(drugs$AGE2, 8) #8 bins - unnamed because 8 is the default 2nd argument
hist(drugs$AGE2, 8, main="Hist of Age") #name main because it is not the 3rd argument by default

#ok to use combo of named and unnamed arguemnts
#order doesn't matter if they're all named

library(dplyr)

count(drugs, CIGEVER) #don't need to use $ syntax with dyplyr
drugs %>% count(CIGEVER) #same result using chain operator

drugs_lim <- drugs %>% 
              select(CIGEVER) %>% 
              filter(CIGEVER=="Yes")



