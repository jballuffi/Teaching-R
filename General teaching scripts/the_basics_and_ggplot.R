

####This is a script to start learning R and ggplot


#read in the package ggplot 2
library(ggplot2)


#read in data ----
DT <- mtcars

##ways to look at the data
head(DT) #head function lets me see first 5 lines of data
str(DT)   #str function tells me how each column of data is classified
print(DT)  #print function will just print the entire datasheet
class(DT)

#make a new column that classifies gear as a factor
DT$gear2 <- as.factor(DT$gear)



#calculate mean horse power of cars and name the mean
meanhp <- mean(DT$hp)
meanhp * 5

calculation <- (mean(DT$hp))*5


#calculates standard deviation of horse power and name it
sdhp <- sd(DT$hp)


lm(DT$mpg ~ DT$cyl)
lm(mpg ~ cyl, data = DT)


model1 <- lm(mpg ~ cyl + hp, DT)
summary(model1)

DT$cyl <- as.factor(DT$cyl)

ggplot(DT)+
  geom_point(aes(y = mpg, x = hp, color = cyl, size = wt), alpha = .5)+
  labs(x = "horse power")+
  ylim(0, 40)+
  xlim(0, 400)+
  themeblank
 




### create ggplot ----

#this is an example of how to make your own ggplot theme
#if you want to add the theme to the ggplot just write in the theme name
themeblank <- theme(
  axis.title = element_text(size = 14),
  axis.text.x = element_text(size = 10),
  legend.key = element_blank(),
  legend.position = "none",
  panel.background = element_blank(),
  panel.border = element_rect(
    colour = "black",
    fill = NA,
    size = 1
  )
)


#create a ggplot and give it a name so you can save it later
#a named ggplot will not show up in the plots tab unless you wrap it in ()

plot1<- ggplot(data = DT) +  #call in datasheet
    geom_smooth(aes(x = hp, y = mpg)) + #creates a smooth line with confidence band/ribbon
    geom_point(aes(x = hp, y = mpg), size = 2, alpha = .5) +  #plot points of mpg against hp
    facet_wrap(. ~ gear2) +   #facet wrap to create three panels of the same plot, one for each gear
    labs(x = "horse power", y = "miles per gallon", title = "cars") +  #change axis titles
    xlim(50, 300) +  #change limits of the x-axis
    theme_minimal()

 
 
 
 #add in a pre-set ggplot theme


#save the ggplot into a folder in your project called 'figures'

ggsave(filename = "Output/hp_fig.jpeg", 
  plot1,
  width = 7,
  height = 4,
  units = "in"
)
