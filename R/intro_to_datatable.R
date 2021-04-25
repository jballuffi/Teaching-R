# script for learning data.table

library(data.table)
library(lubridate)

#fread is an efficient way to read in data that's automatically classified as a data.table
f <- fread("Input/foraging_data.csv")
#alternatively you can convert a data.frame into a data.table with the function as.data.table()
c <- as.data.table(mtcars)


#syntax for data.table:

# DT[i, j, by]

# [with i, do j, by group]

### once you learn this you are set!



# Examples of with I's ----

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


# Examples of do J's ----

#calculate mean gear with all data
c[, mean(gear)]

#with vs of 0, calculate mean gear
c[vs == 0, mean(gear)]

#create a new column to classify cars based on mpg

#with cars that have mpg < 25, classify as a gas guzzler
c[mpg < 25, classification := "gas guzzler"]
#with cars leftover, classify as gas mazer
c[is.na(classification), classification := "gas mazer"]


# Examples of By's----

#calculate mean mpg by gear
c[, mean(mpg), by = gear]

#save mean mpg by gears as new datatable
MeanMPG <- c[, mean(mpg), by = gear]
setnames(MeanMPG, "V1", "Meanmpg")

#calc mean and sd for mpg by gear, save as new datatable
MPG <- c[, .(mean(mpg), sd(mpg)), by = gear] #the period is needed!
setnames(MPG, c("V1", "V2"), c("Meanmpg", "SDmpg"))


#Example of a complete i, j, by----

#with gas guzzlers, calculate mean disp, by gear
c[classification == "gas guzzler", mean(disp), by = gear]

#next steps: 
#working with multiple columns simultaneously
#merge function
#write your own function, or model, and run by group
#lapply
#melt 
#dcast
#tstrsplit


#date time with lynx data ----

#separate the date time column and keep date
lynx[, Date := tstrsplit(datetime, " ", fixed = TRUE, keep = c(1))]

#separate the datetime column and keep time
lynx[, Time := tstrsplit(datetime, " ", fixed = TRUE, keep = c(2))]

#separate the date column and keep year
lynx[, Year := tstrsplit(Date, "-", fixed = TRUE, keep = c(1))]

#reclassify the dates and times
lynx[, idate := ymd(Date)] #create idate
lynx[, itime := hms(Time)] #create itime
lynx[, datetime := ymd_hms(datetime)] #create datetime
lynx[, month := month(idate)] #create a month


#now save this new version of the lynx data as an RDS file
## this will maintain the date and time classifications when you load this data the next time
saveRDS(lynx, "Input/lynxcleaned.rds")
