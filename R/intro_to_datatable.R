# script for learning data.table
# Author: Juliana Balluffi-Fry

library(data.table)
library(lubridate)
library(ggplot2)

#fread is an efficient way to read in data that's automatically classified as a data.table
f <- fread("Input/foraging_data.csv")
#alternatively you can convert a data.frame into a data.table with the function as.data.table()
c <- as.data.table(mtcars)


#syntax for data.table:

# DT[i, j, by]

# [with i, do j, by group]

### once you learn this you are set!




# Examples of the I's -----------------------------------------------------

#grab rows where car had 3 gears
c[gear == 3]

#grab rows with more than 3 gears
c[gear > 3]

#grab rows with any number other than 3 gears (two options)
c[gear != 3]

#create new data.table with subset data
threegear <- c[gear == 3]

#subset for cars with 3 or 4 gears
c[gear == 3 | gear == 4]

#subset for cars with 3 gears and VS of 0
c[gear == 3 & vs == 0]



# Examples of the J's -----------------------------------------------------

#calculate mean gear with all data
c[, mean(gear)]

#with vs of 0, calculate mean gear
c[vs == 0, mean(gear)]

#create a new column to classify cars based on mpg
#with cars that have mpg < 25, classify as a gas guzzler
c[mpg < 25, classification := "gas guzzler"]

#with cars leftover, classify as gas mazer
c[is.na(classification), classification := "gas mazer"]



# Examples of the by's ----------------------------------------------------

#you need to have at least a j to do a by

#calculate mean mpg by gear
c[, mean(mpg), by = gear]

#count number of data points by gear
c[, .N, by = gear]

#save mean mpg by gears as new datatable
MeanMPG <- c[, mean(mpg), by = gear]
setnames(MeanMPG, "V1", "Meanmpg")

#calc mean and sd for mpg by gear, save as new datatable
MPG <- c[, .(mean(mpg), sd(mpg)), by = gear] #the period is needed!
setnames(MPG, c("V1", "V2"), c("Meanmpg", "SDmpg"))



# Example of a complete 'with i, do j, by group' --------------------------

#with 4-cylinder cars, count number of data, by gear
c[cyl==4, .N, by = gear]

#with gas guzzlers, calculate mean disp, by gear
c[classification == "gas guzzler", mean(disp), by = gear]



# More advanced techniques ------------------------------------------------
# Here we will be using some fabricated foraging data
f <- fread("Input/foraging_data.csv")

f[, .N]
f[, unique(ID)]
f[, unique(ID), by = habitat]
f[, .N, by = ID]

f[, mean(graze)]
f[, mean(browse)]

f[, unique(graze_unit)] #check out grazing units
f[, unique(browse_unit)] #check out grazing units




#units aren't the same, lets convert using data.table
f[graze_unit=="kg/day", graze := graze*1000][, graze_unit := "g/day"]

f[graze_unit=="kg/day", c('graze', 'graze_unit') := .(graze*1000, "g/day")]

f[graze_unit=="kg/day"|browse_unit=="kg/day",
  c('graze', 'browse', 'graze_unit', 'browse_unit') 
  := .(graze*1000, browse*1000, "g/day", "g/day")]

un <- grep("unit", names(f), value=TRUE)

f[grepl("kg", list(un))]

f[grepl("kg", graze_unit)|grepl("kg", browse_unit),
  c('graze', 'browse', 'graze_unit', 'browse_unit') 
  := .(graze*1000, browse*1000, "g/day", "g/day")]


#create an idate
f[, date := ymd(date)]

#create a year column
f[, year := year(idate)]

f[, year2 := tstrsplit(idate, "-", keep=1)]


f[, mean(browse), by = ID]
f[, mean(graze), by = habitat]
test<-f[, .(mean(graze), sd(graze)), by = c('ID', 'habitat', 'region')]
setnames(test, c('V1', 'V2'), c('mean_graze', 'sd_graze'))

f[, mean_graze := mean(graze), by = ID]

#issue with this formate when plotting
ggplot(f)+
  geom_point(aes(x=date, y=graze))+
  geom_point(aes(x=date, y=browse), color="blue3")

#use the melt function
# measure.vars = the columns you want to collapse into onw
# value.name = what this new column will be called
# variable.name = name of the column that categorizes them

feed <- melt(f, measure.vars = c("browse", "graze"), value.name = "feedrate", variable.name = "foodtype")
feed[, .N] #our new data.table has 400 rows, double before
feed[foodtype=="graze", foodtype := "grass"]
feed[, c("browse_unit", "graze_unit"):=NULL]

ggplot(feed)+
  geom_point(aes(x=date, y=feedrate, color=foodtype))




rsq(lm(feedrate~foodtype + date, data=feed))

coef(lm(feedrate~foodtype + date, data=feed))

feed[, coef(lm(feedrate~foodtype + date))]
feed[ID=="D", coef(lm(feedrate~foodtype + date))]
feed[, coef(lm(feedrate~foodtype + date)), by=ID]
mods<- feed[, data.table(t(coef(lm(feedrate~foodtype + date)))), by=ID]



CNRSF <- function(yvar, xvar1, xvar2) {
  # Make the model
  model <- lm(yvar ~ xvar1 + xvar2)
  # Transpose the coef of the model and cast as data.table
  coefOut <- data.table(t(coef(model)))
  # Transpose the standard error coef of the model and cast as data.table
  secoefOut <- data.table(t(se.coef(model)))
  #label the column name for the rsqOut
  names(rsqOut)<-c("rsq")
  # Add 'se-' prefix to standard error coef table
  setnames(secoefOut, paste0('se-', colnames(secoefOut)))
  # Return combined columns
  return(data.table(coefOut, secoefOut, rsqOut))
}

#write your own function, or model, and run by group
#lapply
#dcast



#date time with lynx data ----



