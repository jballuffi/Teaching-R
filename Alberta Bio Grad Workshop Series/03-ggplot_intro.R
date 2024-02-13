
#workshop 3 of the Alberta bio grad R workshop series
#last updates February 13th 2024
#author: Juliana Balluffi-Fry

# this script introduces ggplot and how to use the tool efficiently and reproducibly

#load packages
library(data.table)
library(ggplot2)
library(ggpubr) #for making multipanel figures
library(ggeffects) #for the ggpredict function

#read in data
#convert mtcars into a data.table
cars <- as.data.table(mtcars)



# how to use ggplot syntax -------------------------------------------------

#ggplots work as layers. Add a "+" at the end of your line to add more

# ggplot(data that will be used for the whole figure)+
# geom_point(aes( x, y... anything about the data representation), anything about the presentation or specify data source)
# additional things...



# make some basic ggplots -------------------------------------------------

#miles per gallon as a response to horse power
ggplot(cars)+
  geom_point(aes(x = hp, y = mpg))

#change axis labels
ggplot(cars)+
  geom_point(aes(x = hp, y = mpg))+
  labs(x = "horse power", y = "miles per gallon")

#change the theme
ggplot(cars)+
  geom_point(aes(x = hp, y = mpg))+
  labs(x = "horse power", y = "miles per gallon")+
  theme_minimal()

#change the presentation of the data points
ggplot(cars)+
  geom_point(aes(x = hp, y = mpg), size = 2, alpha = 0.5, color = "blue4")+
  labs(x = "horse power", y = "miles per gallon")+
  theme_minimal()


#to help streamline the figure making process: make your own theme!!
#save the theme at the top of your script or in another script
#use the same custom theme for all your figures
#you can pretty much customize everything within the theme
theme1 <- theme(axis.title = element_text(size = 13),
                     axis.text = element_text(size = 10),
                     legend.position = "top",
                     legend.key = element_blank(),
                     legend.title = element_blank(),
                     panel.background = element_blank(),
                     axis.line.x.top = element_blank(),
                     axis.line.y.right = element_blank(),
                     axis.line.x.bottom = element_line(size = .5),
                     axis.line.y.left = element_line(linewidth = .5),
                     panel.border = element_blank(),
                     panel.grid.major = element_line(linewidth = 0.5, color = "grey90"))

#to see all the things you can change 
?theme

#lets use this custom made theme
ggplot(cars)+
  geom_point(aes(x = hp, y = mpg))+
  labs(x = "horse power", y = "miles per gallon")+
  theme1




# adding complexity: statists ---------------------------------------------

#geom smooth is a quick and dirty way to get a line through your data
ggplot(cars)+
  geom_point(aes(x = hp, y = mpg))+
  geom_smooth(aes(x = hp, y = mpg))+
  labs(x = "horse power", y = "miles per gallon")+
  theme1

#to make it a linear relationship you must specify with the 'method' argument
ggplot(cars)+
  geom_point(aes(x = hp, y = mpg))+
  geom_smooth(aes(x = hp, y = mpg), fill = NA, method = "lm")+
  labs(x = "horse power", y = "miles per gallon")+
  theme1

#however, I like having full control
#like if I want to show a relationship from a model that has other terms for example

#how do we reproducibly make a model, and send the output to a ggplot? 

#step 1: create the linear relationship
mod <- lm(mpg ~ hp, cars)
summary(mod)

#pull the slope and intercept of this linear regression using the coef() function
slopemod <- coef(mod)["hp"]
intmod <- coef(mod)["(Intercept)"]

#now using the function abline, we can recreate this regression in the ggplot
ggplot(cars)+
  geom_point(aes(x = hp, y = mpg))+
  geom_abline(intercept = intmod, slope = slopemod)+
  labs(x = "horse power", y = "miles per gallon")+
  theme1

#the non-reproducible way would be to input the slope and intercept directly (hard coding)
# DO NOT DO THE FOLLOWING
ggplot(cars)+
  geom_point(aes(x = hp, y = mpg))+
  geom_abline(intercept = 30.09886, slope = -0.06823)+
  labs(x = "horse power", y = "miles per gallon")+
  theme1

#add a horizontal line to represent some threshold
ggplot(cars)+
  geom_point(aes(x = hp, y = mpg))+
  geom_abline(intercept = intmod, slope = slopemod)+
  geom_abline(intercept = 15, slope = 0, linetype = 3)+
  labs(x = "horse power", y = "miles per gallon")+
  theme1



# using ggpredict for interactions  ---------------------------------------

#I want to know how 'am' and 'hp' interact to affect 'mpg'

#convert the column 'am' to a factor
cars[, am := as.factor(am)] #data.table syntax
#cars$am <- as.factor(cars$am)  #base R syntax

#create a model with an interaction
mod2 <- lm(mpg ~ hp*am, mtcars)
summary(mod2)

#use ggpredict (from ggeffects package)
#first argument is the model
#second argument are the terms in the model you are interested in
#wrap in data.table if you like
effsmod2 <- as.data.table(ggpredict(mod2, terms = c("hp", "am")))

#the effects show a 'predicted' value (mpg) in response to another value called x (hp)
#and the prediction is grouped by another variable (am) in the 'group' column 

#add this prediction as a line and a ribbion in your figure
#you have to specify two data frames now, so keep the line, ggplot(), empty
ggplot()+
  geom_line(aes(x = x, y = predicted, color = group), data = effsmod2)+
  geom_ribbon(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.5, data = effsmod2)+
  geom_point(aes(x = hp, y = mpg, color = am), size = 2, data = cars)+
  labs(x = "horse power", y = "miles per gallon")+
  theme1

#this is a great way to have full control over exactly what model you show and how



# saving a ggplot ---------------------------------------------------------

#must name the figure 
(ggplot1 <- ggplot()+
  geom_line(aes(x = x, y = predicted, color = group), data = effsmod2)+
  geom_ribbon(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.5, data = effsmod2)+
  geom_point(aes(x = hp, y = mpg, color = am), size = 2, data = cars)+
  labs(x = "horse power", y = "miles per gallon")+
  theme1)
#wrap the named figure in () to show it when you run the line

#save using ggsave
ggsave("Output/carsfigure.jpeg", ggplot1, width = 5, height = 4, unit = "in")

