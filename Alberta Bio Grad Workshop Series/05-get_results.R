#script for extracting model outputs and exporting results tables. 
#author: Juliana Balluffi-Fry

#there are a number of packages that are used today
library(data.table)
library(ggplot2)
library(dplyr)

#these three packages are specialized for model results
library(AICcmodavg) #use to create AIC tables
library(rsq) #extract R2s from various types of models
library(arm) #this package is for regression and multilevel/hierarchical Models, but
#here I use the arm package for it's function se.coef that extracts standard errors of coef

#load biomass data 
bio <- fread("Input/biomass.csv")



# create a basic linear regression and explore the output -----------------

#make a basic linear regression 
mod <- lm(bio$shrub_biomass ~ bio$Julian_day)

#if this model uses a lot of data and computing power, I suggest saving as RDS
saveRDS(mod, "Output/model.rds") #save as rds
mod2 <- readRDS("Output/model.rds") #read in later if you want to pick it up again

#normally people use the following functions and copy and paste the outputs somewhere else
summary(mod)
anova(mod)

#the function coef will return the coefficients (slopes) and intercept
coef(mod)

#we can pull out the slope and intercept of this regression using coef()
s <- coef(mod)["bio$Julian_day"]
int <- coef(mod)["(Intercept)"]

#now you can input the slope and intercept directly into a ggplot
ggplot(bio)+
  geom_point(aes(x = Julian_day, y = shrub_biomass))+
  geom_abline(aes(intercept = int, slope = s)) 

#using a function from the 'arm' package, we can also extract the se.coef
se.coef(mod)
se <- se.coef(mod)["bio$Julian_day"]



# create a function that extracts slopes to run in data.table -------------

#this function builds a model, extracts the slope, and then returns the slope
get_slope <- function(x, y){
  model <- lm(y ~ x)
  slope <- coef(model)["x"]
  return(slope)
}

#run the model by year and drainage class 
bio[, get_slope(x = Julian_day, y = shrub_biomass), by = .(year, drainage_class)]

#in reality we don't really need this function, you could write it in one line
#but it's a good place to start
bio[, coef(lm(shrub_biomass ~ Julian_day))["Julian_day"], by = .(year, drainage_class)]

# the quoted ["Julian_day"] doesn't need a bio$Julian_day because it's working within the data.table



#  function that extracts multiple model outputs to run in data.table --------

#you will notice that this function does a lot of data.table wrapping, 
#I wrap things in data.table() so that I can work in data.table within the function
#lots of things are also wrapped in t(), this stands for transposed
#transposing the numbers get's them in a wide table format, rather than long
#you can experiment with outputs without t() to see what happens


getmod <- function(x, y) {
  model <- lm(y ~ x) #create model
  
  coefOut <- data.table(t(coef(model))) #collect coef values
  
  coefOut<-round(coefOut, 3) #round coef table 3 decimals
  
  seOut <- data.table(t(se.coef(model))) #collect standard errors
  
  seOut<-round(seOut, 3) #round se to 3 decimals

  coefse<-data.table(t(paste0(coefOut, " ± ", seOut))) #paste coef and se together
  
  setnames(coefse, paste0(colnames(coefOut))) #rename the col names to be the cols of coef table 

  rsqOut <- data.table(rsq(model)) #collect r2s
  
  names(rsqOut)<-c("rsq") #rename col
  
  rsqOut <- round(rsqOut, 3) #round R2
  
  return(data.table(coefse, rsqOut)) #bound the two data.tables together and return
}

#run this large model by year and drainage, save in environment
modoutput <- bio[, getmod(x = Julian_day, y = shrub_biomass), by = .(year, drainage_class)]

#if you want to reorder the columns you can use the function below:
#  setcolorder()

#this could be exported for your manuscript with one extra line
write.csv(modoutput, "Output/biomass_output.csv")


# Ceating an AIC table ----------------------------------------------------


#first make the models, these are the lines you would change if you need to re-run things
Base <- lm(shrub_biomass ~ Julian_day, data = bio)
Interann <- lm(shrub_biomass ~ Julian_day + year, data = bio)
Env <- lm(shrub_biomass ~ Julian_day + drainage_class, data = bio)

#next list the models and list the names of models
#these are also subject to change with re-analysing
#THESE HAVE TO BE IN THE SAME ORDER FOR LATER USE
mods <- list(Base, Interann, Env)
Names<-c("Base", "Interannual", "Environmental")


#now, create the AIC table using aictab()
AIC<-as.data.table(aictab(REML=F, cand.set = mods, modnames = Names, sort = TRUE))
AIC[, ModelLik := NULL] #remove some unnecessary columns
AIC[, Cum.Wt := NULL]
#round whole table to 3 dec places using dplyr (a rare case where I have used dplyr)
AIC<-AIC %>% mutate_if(is.numeric, round, digits=3)



# collecting model outputs from a list of models ----------------------


#If we want to collect other outputs from the models in the AIC
#we need to write a function that can be run using lapply
#so the only argument we write in our function is model

#Function to collect R2s for every model
collectR2 <- function(model) {
  rsqOut <- data.table(rsq(model))  #collect R2s
  rsqOut<- round(rsqOut, 2) #round 
  return(data.table(rsqOut)) #return
}

#run function and get R2s for all models
R2s <- lapply(mods, collectR2)

#rbindlist the output
R2s <- rbindlist(R2s, fill = TRUE)

#rename the one column
names(R2s) <- ("R2")

#add in a column called Modnames, insert the Names string
R2s$Modnames <- Names

#merge R2s with AIC table
AICMS<- merge(AIC, R2s, by="Modnames")


#Function to collect coefficients, standard errors, and R2s for every model
modsoutput <- function(model) {
  #collect coef values
  coefOut <- data.table(t(coef(model)))
  coefOut<-round(coefOut, 3)
  #collect standard errors
  seOut <- data.table(t(se.coef(model)))
  seOut<-round(seOut, 3)
  #Paste coef and standard errors together, rename cols
  coefse<-data.table(t(paste(coefOut, seOut, sep=" ± ")))
  setnames(coefse, paste0(colnames(coefOut)))
  #collect R2s and change column name
  rsqOut <- data.table(rsq(model))
  names(rsqOut)<-c("rsq")
  rsqOut <- round(rsqOut, 3)
  #return each datatable binded together by row
  return(data.table(coefse, rsqOut))
}

#apply to same list of models as in AIC
OutAll<-lapply(mods, modsoutput)
OutAll<-rbindlist(OutAll, fill = TRUE)
OutAll$Model<-Names
