#extracting model outputs and results in a reproduceable way

library(data.table)
library(ggplot2)
library(AICcmodavg)
library(rsq)
library(arm)
library(dplyr)


bio <- fread("Input/biomass.csv")


#how do basic linear models work? 
mod <- lm(bio$shrub_biomass ~ bio$Julian_day)

saveRDS(mod, "Output/model.rds")
mod2 <- readRDS("Output/model.rds")

coef(mod)

slope <- coef(mod)["bio$Julian_day"]
int <- coef(mod)["(Intercept)"]


se.coef(mod)
se <- se.coef(mod)["bio$Julian_day"]



ggplot(bio)+
  geom_point(aes(x = Julian_day, y = shrub_biomass))+
  geom_abline(aes(intercept = int, slope = slope))



get_slope <- function(x, y){
  model <- lm(y ~ x)
  slope <- coef(model)["x"]
  return(slope)
}

bio[, get_slope(x = Julian_day, y = shrub_biomass), by = .(year, drainage_class)]



getmod <- function(x, y) {
  #create model
  model <- lm(y ~ x)
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


modoutput <- bio[, getmod(x = Julian_day, y = shrub_biomass), by = .(year, drainage_class)]









Base <- lm(shrub_biomass ~ Julian_day, data = bio)
Interann <- lm(shrub_biomass ~ Julian_day + year, data = bio)
Env <- lm(shrub_biomass ~ Julian_day + drainage_class, data = bio)


mods <- list(Base, Interann, Env)
Names<-c("Base", "Interann", "Env")


AIC<-as.data.table(aictab(REML=F, cand.set = mods, modnames = Names, sort = TRUE))
AIC[, ModelLik := NULL]
AIC[, Cum.Wt := NULL]
#round whole table to 3 dec places
AIC<-AIC %>% mutate_if(is.numeric, round, digits=3)

#merge with model construction if wanted


#Function to collect R2s for every model
collectR2 <- function(model) {
  #collect R2s
  rsqOut <- data.table(rsq(model))
  rsqOut<- round(rsqOut, 2)
  #return each datatable binded together by row
  return(data.table(rsqOut))
}

#run function and get R2s for all models
R2s <- lapply(mods, collectR2)
R2s <- rbindlist(R2s, fill = TRUE)
names(R2s) <- ("R2")
R2s$Modnames <- Names

#merge R2s with AIC table
AICMS<- merge(AICMS, R2s, by="Modnames")

#creating final format for table 2
Tab3<- AICMS[, .(Modnames, Body, AICc, Delta_AICc, R2m, R2c)]
setnames(Tab3, "Body", "Design")






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
OutAll[,`(Intercept)`:=NULL]



#function to swap out specific words in column names for new ones
nameswap <- function(old, new, Data) {
  older<-colnames(Data)[grep(old, colnames(Data))]
  newer <- gsub(old, new, older)
  setnames(Data, older, newer)
}



#now reorder the cols
setcolorder(OutAll, c("Julian_day"...))
