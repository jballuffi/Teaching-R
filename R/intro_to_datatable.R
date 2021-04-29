# script for learning data.table
# Author: Juliana Balluffi-Fry

library(data.table)
library(lubridate)
library(ggplot2)

#fread is an efficient way to read in data that's automatically classified as a data.table
f <- fread("Input/foraging_data.csv")
#alternatively you can convert a data.frame into a data.table with the function as.data.table()
cars <- as.data.table(mtcars)


#syntax for data.table:

# DT[i, j, by]

# [with i, do j, by group]

### once you learn this you are set!




# Examples of the I's -----------------------------------------------------

#grab rows where car had 3 gears
cars[gear == 3]

#grab rows with more than 3 gears
cars[gear > 3]

#grab rows with any number other than 3 gears (two options)
cars[gear != 3]

#create new data.table with subset data
threegear <- cars[gear == 3]

#subset for cars with 3 or 4 gears
cars[gear == 3 | gear == 4]

#subset for cars with 3 gears and VS of 0
cars[gear == 3 & vs == 0]



# Examples of the J's -----------------------------------------------------

#count the number of rows in the data
#.N is a special symbol in data.table, it counts the number of rows
#.N is not a function, you can't apply it to anything specific (e.g. this wouldn't work: .N(gear))
#other examples of species symbols .SD, .BY, .I, but we wont get into those today
cars[, .N]

#calculate mean gear with all data
cars[, mean(gear)]

#with vs of 0, calculate mean gear
cars[vs == 0, mean(gear)]

#create a new column to classify cars based on mpg
#with cars that have mpg < 25, classify as a gas guzzler
cars[mpg < 25, classification := "gas guzzler"]

#with cars leftover, classify as gas mazer
cars[is.na(classification), classification := "good car"]



# Examples of the by's ----------------------------------------------------

#you need to have at least a j to do a by

#calculate mean mpg by gear
cars[, mean(mpg), by = gear]

#count number of rows by gear
cars[, .N, by = gear]

#save mean mpg by gears as new data.table
MeanMPG <- cars[, mean(mpg), by = gear]

#use the data.table function, setnames to change the V1 column name
setnames(MeanMPG, "V1", "Meanmpg")

#calc mean and sd and max value for mpg by gear, save as new data.table
#the period before the list of j's is a data.table shortcut for 'list'
MPG <- cars[, .(mean(mpg), sd(mpg), max(mpg)), by=gear]

#change column names
setnames(MPG, c("V1", "V2", "V3"), c("mean", "sd", "max"))




# Example of a complete 'with i, do j, by group' --------------------------

#with 4-cylinder cars, count number of data, by gear
cars[cyl==4, .N, by = gear]

#with gas guzzlers, calculate mean disp, by gear
cars[classification == "gas guzzler", mean(disp), by = gear]



# More advanced techniques ------------------------------------------------
# Here we will be using some fabricated foraging data
# each row of data represents a day of feeding by a herbivore


##First lets do some basic data exploration

#get length of the whole data.table
f[, .N]

#check out what individuals are in this data
f[, unique(ID)]

#what habitats are we dealing with?
f[, unique(habitat)]

#are all individuals in all habitats?
f[, unique(ID), by = habitat]

#how many rows of data exist for each individual?
f[, .N, by = ID]

#return mean grazing amounts and mean browsing amounts
f[, .(mean(graze), mean(browse)), by=region]

#what units were grazing and browsing measured in?
f[, .(unique(graze_unit), unique(browse_unit))]



# Unit conversions --------------------------------------------------------

#okay so we have different units, this means we need to convert the graze and browse columns,
#then we should change the corresponding values in the units columns to match
#we want everything to be in g/day

#below we will see a couple options on how to solve this problem
#reload the data.table here to start fresh
f <- fread("Input/foraging_data.csv")

#option 1:
#this option uses data.table's version of piping: dt[i, j , by][i, j, by]
#here we take rows that have a unit in kg/day (i) and multily the feed column by 1000 (j)
#then immediately after we change the whole unit column to be g/day (the piping)
f[graze_unit=="kg/day", graze := graze*1000][, graze_unit := "g/day"]
f[browse_unit=="kg/day", browse := graze*1000][, browse_unit := "g/day"]

#the issue with the above option is what if the data has more than two units?
#we wouldn't be able to convert the whole column to one unit after we do one conversion

#option 2: work with two or more columns simultaneously
#this option reassigns the unit column but only on the original subset of rows (i is graze_unit=='kg/day')
#if there was a 3rd unit, like "bites/day", it would remain untouched
f[graze_unit=="kg/day", c('graze', 'graze_unit') := .(graze*1000, "g/day")]
f[browse_unit == "kg/day", c('browse', 'browse_unit') := .(browse*1000, "g/day")]

#if you want to make that one line:
f[graze_unit=="kg/day"|browse_unit=="kg/day",
  c('graze', 'browse', 'graze_unit', 'browse_unit') 
  := .(graze*1000, browse*1000, "g/day", "g/day")]

#hypothetical situation: what if we 3 units with different denominators? like kg/day and g/day and kg/week 

#option 3: incorporating the grepl and sub functions
#by using grepl in our i, we would be able to change all units with a kg, no matter the unit denominator
#and then we can use the sub function in our j to swap out those kg for g
f[grepl("kg", graze_unit), c('graze', 'graze_unit') := .(graze*1000, sub("kg", "g", graze_unit))]
f[grepl("kg", browse_unit), c('browse', 'browse') := .(browse*1000, sub("kg", "g", browse))]



# working with dates ------------------------------------------------------

#create an idate
#ymd is a lubridate function
f[, date := ymd(date)]

#year is also a lubridate function
#create a year column
f[, year := year(date)]

#alternative solution to creating year using the data.table function tstrsplit
#take the date column, split at the "-", and keep the 1st split item (year)
f[, year2 := tstrsplit(date, "-", keep=1)]



# merging data.tables -----------------------------------------------------

#lets say someone hands you a separate spreadsheet with all the individuals and their sexes
#hypothetical sex spreadsheet:
sexes<- data.table(
  ID = c("D", "C", "B"),
  sex = c("M", "M", "F")
)

#how could we merge this information onto our feeding data? 
#solution: data.table's merge function

#list tables you want to merge as x and y (here x = f and y = sexes)
#by argument is the column you wish to merge by (alternatively by.x and by.y)
#"all = TRUE" means keep all rows of data
#"all.x or all.y = TRUE" means keep all rows of one or the other table

#in this case we are merging the two data.tables by ID, and we want to keep all the rows in f data.table
f <- merge(f, sexes, by = "ID", all.x = TRUE)



# data restructuring ------------------------------------------------------

#there is clearly a difference in how much the herbivore feeds on each food type
f[, .(mean(browse), mean(graze))]

#but this data is not structured correctly
#we currently wouldn't be able to put food type in a model
#we also cannot ggplot the relationship correctly (can't make a legend)

ggplot(f)+
  geom_point(aes(x=date, y=graze))+
  geom_point(aes(x=date, y=browse), color="blue3")

#solution: the melt function 

# measure.vars = the columns you want to collapse into one
# value.name = what this new column will be called
# variable.name = name of the column that categorizes them

#we are collapsing the browse and graze columns into one and naming this new column 'feedrate'
#the names of the original column will become factors in the new column 'foodtype'
feed <- melt(f, measure.vars = c("browse", "graze"), value.name = "feedrate", variable.name = "foodtype")

feed[, .N] #our new data.table has 400 rows, double before

#lets change the graze classification of food to grass
feed[foodtype=="graze", foodtype := "grass"]

#we can delete the browse and graze units if we'd like
feed[, c("browse_unit", "graze_unit"):=NULL]

#now we can correctly plot this in ggplot and assign foodtype as a color
ggplot(feed)+
  geom_point(aes(x=date, y=feedrate, color=foodtype))



# applying linear models to data using the 'by' ----------------------

#here we are interested in the effect of food type and region on feeding rate
#like in this model
summary(lm(feedrate ~ foodtype + region, data = feed))

#but we want to run this model by ID or by year and get multiple model outputs

#this line extracts the model coefficients, transposes them (t), and outputs them as a data.table 
#models are run by ID
feed[, data.table(t(coef(lm(feedrate ~ foodtype + region)))), by = ID]

#next we can do the same but run models by year and by ID
feed[, data.table(t(coef(lm(feedrate ~ foodtype + region)))), by = .(ID, year)]

#If we want to get fancy we can make our own function that extracts more information from models
#then we can run our function by the group of our choice

#requires rsq package and arm package
library(rsq)
library(arm)

#this function extracts coef, se, R2s, and creates a table of this output
makemod <- function(yvar, xvar1, xvar2) {
  # Make the model
  model <- lm(yvar ~ xvar1 + xvar2)
  # Transpose the coef of the model and cast as data.table
  coefOut <- data.table(t(coef(model)))
  # Transpose the standard error coef of the model and cast as data.table
  secoefOut <- data.table(t(se.coef(model)))
  #extract r-squared from model
  rsqOut <- data.table(rsq(model))
  #label the column name for the rsqOut
  names(rsqOut)<-c("rsq")
  # Add 'se-' prefix to standard error coef table
  setnames(secoefOut, paste0('se-', colnames(secoefOut)))
  # Return combined columns
  return(data.table(coefOut, secoefOut, rsqOut))
}

#run the function by ID
modout<- feed[, makemod(yvar=feedrate, xvar1=foodtype, xvar2=region), by=ID]



# Additional example of how to use data.table to calculate proportion --------

#no data here just a code  example
#this would work on observation data, where each row of data is a species observation

#sum number of observations by species code and latitude group
DT2 <- DT[, .N, by=c('species', 'lat_group')] #this will create a column called N

#divide the sum of each individual species by the sum of all observation in a lat group
DT2[, props := N/sum(N), by=lat_group] #creates a new column called props



