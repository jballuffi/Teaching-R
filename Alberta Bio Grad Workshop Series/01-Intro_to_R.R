
#workshop 2 of the Alberta bio grad R workshop series

#Last updated: Oct 27, 2022

#Author: Liam H

#Getting set up ----------------------------------

#Step1 - Create a R project!
# File -> New Project -> New Directory 
# (or File -> New Project -> Existing Directory (if creating a project from existing folders)

#Click white sheet in top left -> New R Script

#Importing data ---------------------------------

#import dataset with rows and columns and save it as a 'data frame'
# "<-" is how you save and name something in R (will appear in the Environment (top right))
#mtcars is an example dataset stored within R
DT <- mtcars #CRTL + ENTER to run a line of code (COMMAND + ENTER for Macs)

#Import data from a csv (put the csv in a folder WITHIN your project - mine is called 'data')
anyname <- read.csv("data/data.file.2022") #hit 'Tab' key after / to show all data within that folder


#Using built-in functions to look at data ---------------------------------

#Any function in R has:
#the function name followed by parentheses (which contain the arguments)
#commas to separate the arguments (if multiple)

#Look up a basic function to understand its purpose and arguments
?head #head(x, ...)
#Scroll down to "Arguments"
#Only the 'x' argument is required (the other arguments have defaults and are therefore optional)


#ways to look at the data

head(x = DT) #head function lets me see first 5 lines of data
head(x = DT, n = 7) #optional 'n' argument indicates how many rows to show
head(DT, 7) #if you know the order of the arguments, you can just use the desired values
head(7, DT) #will give an error

str(DT)   #str function tells me how each column of data is classified
#all columns are numerical (num) - Google the classes of your own data to understand them

print(DT)  #print function will just print the entire datasheet

View(DT)  #pulls up data sheet in a new tab where columns can be sorted
          #can also do this by clicking on DT in the Environment (top right)

class(DT)  #how your entire dataset is classified (data frame)


#Specifying a column ------------------------------------------

#make new col that classifies gear as a 'factor' (a categorical data type)
#You might need to do this if you want 'gear' to be a categorical variable for a plot
# '$' is R-language for specifying a specific column within a data frame

DT$gearf <- as.factor(DT$gear) #If named the new column 'gear' instead of 'gearf'
                                #it would overwrite the gear column
class(DT$gearf) #factor
class(DT$gear)  #numeric

#calculate mean horse power of cars and name the mean
?mean

mean(DT$hp) #output below, but not saved anywhere
meanhp <- mean(DT$hp) #save it as a single value called "meanhp"

#mutiply the mean by 5 (just like a calculator)
meanhp * 5

#Do this all in one line and name the output value as "calc"
calc <- (mean(DT$hp) * 5)


# Installing and loading packages -----------------------

#install

install.packages("ggplot2") #must use quotations 
#can also click 'Packages' -> 'Install' (on the bottom right window)
#the package is now on your computer until you remove it

#loading

library(ggplot2) #don't use quotations
#need to do this on EVERY script if you want to use this package
#best to load all your packages at the TOP of each script
#can also click 'Packages' -> click check boxes of any packages you want to load

