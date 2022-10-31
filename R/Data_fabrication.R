#this script creates some fake foraging data

library(data.table)



# create data from scratch ------------------------------------------------


#number of rows you wnat in the dataset
nrows<-200

#list different habitat types you observe forager in
hablist<-c("open", "forest", "shrub", "bog")

#create data.table, each item equals one column, data.table creates a list of columns
#this data shows the observed daily intake rates of a herbivore species
#in addition, each data point represents a day of foraging for a certain individual
Data<-data.table(
  #grazing rates, two sets pasted together
  graze = c(runif(nrows/2, 1, 2), runif(nrows/2, 1000, 2000)),
  #units of grazing rates, measured in g and kg
  graze_unit = rep(c("kg/day", "g/day"), each = nrows/2), 
  #browsing rates, two sets
  browse = c(runif(nrows/2, 2, 3), runif(nrows/2, 2000, 3000)), 
  #units of browsing rates, measured in g and kg
  browse_unit = rep(c("kg/day", "g/day"), each = nrows/2), 
  #data points are paired to one of 5 ind. randomly
  ID = sample(LETTERS[1:5], replace = TRUE), 
  #sequence of dates
  date = seq.Date(as.Date("2018/01/01"), as.Date("2021/01/01"), length.out = nrows), 
  #randomly draw temperature from 0 to 25 deg C
  temp = runif(nrows, 0, 25), 
  #randomly sample from habitat list
  habitat = sample(hablist, replace = TRUE), 
  #randomly assign binary social factor- 1 = solo, 2 = grouped
  social = sample(1:2, replace = TRUE), 
  #randomly assign one of two regions
  region = sample (1:2, replace = TRUE) 
)



# add to imported feeding trial data --------------------------------------

ft <- fread("Input/feeding_trials.csv")

ft[, End_1 := sample(50:75, size = nrow(ft), replace = TRUE)]
ft[, End_2 := sample(70:80, size = nrow(ft), replace = TRUE)]

ft[Diet == "A", Performance := sample(20:26, size = nrow(.SD), replace = TRUE)]

ft[Diet == "B", Performance := sample(27:32, size = nrow(.SD), replace = TRUE)]

ft[Diet == "C", Performance := sample(33:37, size = nrow(.SD), replace = TRUE)]

inds <- c("An_1", "An_2", "An_3")

test <- ft[ID %in% inds]



#save as CSV file into input folder
write.csv(Data, file = "Input/foraging_data.csv")

write.csv(ft, file = "Input/feeding_trials_workshop.csv")
