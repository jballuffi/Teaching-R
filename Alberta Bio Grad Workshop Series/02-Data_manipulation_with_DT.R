
#workshop 2 of the Alberta bio grad R workshop series

#last updated oct 31 2022

#author: juliana balluffi-fry

# this script is an intro to data.table and data manipulation 
# two datasets are needed to work through the script (see input folder)


#load in packages
library(data.table)
library(ggplot2)

#read in data using the data.table function, fread()
ft <- fread("Input/feeding_trials_workshop.csv")
diets <- fread("Input/Diets.csv")

# the feeding trials are results for individual animal performace on 3 different diets
# each diet was fed to the animal for 2 days, and one performance was measured at the end 
# one goal is to plot performance against protein intake

#using the function class, we can see that the data is classed as a data.table
class(ft)

#if you don't read the data in with fread, and thus have a dataframe. Convert to data.table
mtcars <- as.data.table(mtcars)
class(mtcars)


# syntax for data.table:

# DT[i, j, by]

# [with i, do j, by group]

### once you learn this you are set!

# the i subsets the data by row
# the j calls on a function or action
# the 'by group' allows you to do your j by a categorical variable in your data




# examples of I's ---------------------------------------------------------

#subset new data to just results from diet A
dietA <- ft[Diet == "A"]

#show rows where performance is higher than 36
ft[Performance > 36]

#show data from diet A or diet B. "|" represented or
ft[Diet == "A" | Diet == "B"]

#show data for cases of very high or very low performance
ft[Performance > 36 | Performance < 20]

#show data for diet A and performance greater than 22
ft[Diet == "A" & Performance > 22]

#list some individuals of interest
inds <- c("An_1", "An_2", "An_3")

#show data for just those individuals
ft[ID %in% inds]

#show any cases that have an NA for diet. Should be none
ft[is.na(Diet)]




# examples of the J's -----------------------------------------------------

#calculate the median performance
medper <- ft[, median(Performance)]

#calculate performance for just diet A
ft[Diet == "A", mean(Performance)]

#calculate the minimum, maximum, and median performance
ft[, .(min(Performance), max(Performance), median(Performance))]

#create a new column that categorizes the animals based on performance
#when performance is above or equal to the median, classify the animal as being in good condition
ft[Performance >= medper, status := "good"]

#when performance is below the median, classify the animal as being in poor condition
ft[Performance < medper, status := "poor"]



# examples of by's --------------------------------------------------------

#calculate mean by diet
ft[, mean(Performance), by = Diet]

#caclulate mean and sd by diet, save as separate data
stats <- ft[, .(mean(Performance), sd(Performance)), by = Diet]

#same code but assign column names
stats <- ft[, .(mean = mean(Performance), sd = sd(Performance)), by = Diet]


# calculate intake rates  -------------------------------------------------

# Intake of day one
ft[, Intake_1 := Start_1 - End_1]

#Intake of day two
ft[, Intake_2 := Start_2 - End_2]

#total intake of both days
ft[, total_intake := Intake_1 + Intake_2]



# merge in nutritional data from diet data.table --------------------------

#using the merge function, merge by diet 
fulldt <- merge(ft, diets, by = "Diet", all.x = TRUE)

#now you can calculate protein intake! make a new column
fulldt[, protein_intake := total_intake*Protein]

#the goal has been reached, plotting performance in response to protein intake
ggplot(fulldt)+
  geom_point(aes(x = protein_intake, y = Performance))



# restructure data --------------------------------------------------------

#what if we want to look at how day (either 1st or 2nd) impacts feeding rate?

#right now the data is in wide form, we need to make the data long
#we need a column called "day" that shows day of trial as a factor

#using the melt function (base R), we can manipulate the data from wide to long

days <- melt(ft, #first argument is the data.frame 
             measure.vars = c("Intake_1", "Intake_2"), #next say what columns you want to combine
             value.name = "Intake_rate", #what is the name of the response (what do the two columns both measure?)
             variable.name = "Day" ) #what is the name of the category?

#lets next use the tstrsplit function to clean up the Day column
#split the Day column by the "_" and keep the second part, the number 1 or 2
days[, Day := tstrsplit(Day, "_", keep = 2)]


ggplot(days)+
  geom_boxplot(aes(x = Day, y = Intake_rate))


#now we can plot intake rate by the day of the trial
ft[, ID_date := paste0(ID, "_", Date)]
#you can see the animals eat more on day 1



