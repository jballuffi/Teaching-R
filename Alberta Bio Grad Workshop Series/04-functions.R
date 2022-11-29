#script for workshop on functions. How to write a function, run it with data.table, and lapply
#author: Juliana Balluffi-Fry

library(data.table)
library(ggplot2)


#read in fake feeding trial data
ft <- fread("Input/feeding_trials_workshop.csv")

#load in plant biomass data
bio <- fread("Input/biomass.csv")

#load in fake foraging data
forage <- fread("Input/foraging_data.csv")



# Basic function synthesis ------------------------------------------------

#we need to calculate the total intake during feeding trials (start_1 - end_1) + (start2 - end_2)
ft[, intake := (Start_1 - End_1) + (Start_2 - End_2)]

#lets use this as an example for building a basic function

#name your function something logical, action focused
#regular parantheses house the arguments
#the curly brackets house the actions
sumtrials <- function (s1, s2, e1, e2){
  day1 <- s1 - e1 #the lines inside the action part of the function stay in the function
  day2 <- e1 - e2 #nothing that gets assigned a name here will save to the global env
  sum <- day1 + day2
  return(sum) #the only things that make it out of the functions are thing you return
}
#when you run this function it will save in your environment

###now lets use the function
#by running just the function and not assigning it, we just return numbers
sumtrials(s1 = ft$Start_1, s2 = ft$Start_2, e1 = ft$End_1, e2 = ft$End_2)

#now we can use the function to make a new column using base R
ft$intake <- sumtrials(s1 = ft$Start_1, s2 = ft$Start_2, e1 = ft$End_1, e2 = ft$End_2)

#if we use data.table to run the function we no longer need to use the $ for each argument
ft[, intake := sumtrials(Start_1, Start_2, End_1, End_2)]



# Switch to biomass data: run function with data.table --------------------

#what if I want to investigate trends within each year and/or age class? 
#to visualize this lets use facet wrap
ggplot(bio)+
  geom_point(aes(x = Julian_day, y = shrub_biomass))+
  geom_smooth(aes(x = Julian_day, y = shrub_biomass), method = "lm")+
  facet_wrap(~drainage_class + year) 
#facet wrap is an easy way to split ggplots by a factor or multiple factors

#remember: a major strength in data.table is the "run-by" argument
bio[, mean(shrub_biomass), by = .(year, drainage_class)]
bio[, period := max(Julian_day) - min(Julian_day), by = .(year, drainage_class)]

#maybe we are tired of writing out these by's all the time
bygroups <- c("year", "drainage_class")

#Now you can change one line to get new groupings throughout code 
bio[, min_biom := min(shrub_biomass), by = bygroups]

#lets use these categories as an example of what we could run a function by within a datatable
#goal: to calculate growth rate, via shrub biomass, in 
#we create a function that calculates rate of change of shrub biomass 

#how do basic linear models work? 
mod <- lm(bio$shrub_biomass ~ bio$Julian_day)
coef(mod)["bio$Julian_day"] #this pulls out the slope of julian day

#function here makes a model with two variables and extracts the slope of x var
get_slope <- function(x, y){
  model <- lm(y ~ x)
  slope <- coef(model)["x"]
  return(slope)
}

#lets try the model with base R syntax
get_slope(y = bio$shrub_biomass, x = bio$Julian_day)

#now run the function by the bygroups
bio[, growthrate := get_slope(x = Julian_day, y = shrub_biomass), by = bygroups]



# What if DT is an argument, how do you run the function within a data.frame --------
# sometimes we write functions that are dependent on functions that have data.frame as an argument

# the handy Data.table tool ".SD" and ".SDcols" can help
# .SD stands for subset 

bio[, .SD] #returns full data.frame

bio[shrub_biomass > 300] #this is how you normally subset rows

bio[, .SD, .SDcols = c("date", "year", "shrub_biomass")] #returns subset of columns

# say this is the action we want to run by group
bio[shrub_biomass > 300, mean(shrub_biomass)]
# obviously we could just add the by's to that statement 
# but let's write a function that does this to learn .sD

max_growth <- function(DT, var, num) {
  dat <- DT[var > num] #subset
  return(dat[, mean(var)]) #calculate mean of that subet
}

# lets test the function with base R
max_growth(DT = bio, var = bio$shrub_biomass, num = 300)

# lets run the function by our groups
# to assign a data.table to the DT argument within a data.table
# we assign .SD to DT. This tells the function to use your bygroup for DT
bio[, max_growth(DT = .SD, var = shrub_biomass, num = 300), by = bygroups]



# lapply ------------------------------------------------------------------

#lapply is used to run a function on a list of objects. It's fairly versatile

#this is especially useful for when you have multiple files to read in, for example
#  one file per individual, or year, or any other sample unit

#let's first make a list
#to use the split function you need a factor (drainage class)
biolist <- split(bio, bio$drainage_class)
class(biolist) #you can see this is classed as a list

#how many rows are in the second object?
nrow(biolist[[2]])

#collect number of rows for each class
lapply(biolist, nrow)

#run a function that sums biomass in each object
# this isn't universal but the function can't have arguments other than the objects of the list 
lapply(biolist, function(x)
{x[, sum(shrub_biomass)]})



# how to use lapply to collect files --------------------------------------
# do this when you have multiple files that you want to read in at once
# code reads in all data for the teaching R project so it doesnt make much sense

#create file path to all csv files in the input folder
files <- dir("Input/", "*.csv", full.names = TRUE)

#fread the list of files using lapply
ls.files<-lapply(files, fread)

#rbind the list of files, create an idcol that will keep the file name (if needed)
#origin col has number at first, based on the number of the file in the list
fulldata <- rbindlist(ls.files, fill = TRUE, use.names = TRUE, idcol = 'origin')

#now re-assign the origin column the file names
fulldata[, origin := factor(origin, labels = basename(files))]



# how we can use lapply and data.table together to investigate effect of sample size  -----------------------
# lets say we want to test how different lengths of time affect biomass averages

#first, lets calculate the total length of the study season by year and drainage class
bio[, daycount := Julian_day - min(Julian_day), by = bygroups]

#make a list of sample efforts, from day one to the length of the longest time frame
effort <- c(1: max(bio$daycount))

#use lapply on the list, effort, to run a custom function
# on any data where day count is less than the effort (n), calculate mean
sampleeffort <- lapply(effort, function(n){ #n represents the list of efforts we made
  bio[daycount < n, mean(shrub_biomass), by = bygroups] #calculate mean shrub biomass
})

#rbindlist and create an idcolumn called effort where n is pasted 
timeeffect <- rbindlist(sampleeffort, idcol = "effort")
setnames(timeeffect, "V1", "Mean_biomass")

#now ggplot this new dervied data and you can see the trend in avg biomass over sample time
ggplot(timeeffect)+
  geom_point(aes(x = effort, y = Mean_biomass))+
  facet_wrap(~ drainage_class + year)
  