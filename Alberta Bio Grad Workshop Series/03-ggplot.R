install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)
library(data.table) #just in case you want to use fread() instead of read.csv()

#read the data
biomass <- read.csv("Input/biomass.csv")
#view data
head(biomass)


# explore data ------------------------------------------------------------

#scatter plot with shrub biomass and canopy cover
ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass)) #aes = aesthetic

#change point size
ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass), cex = 2)

#change point shape
ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass), cex = 2, shape = 5)

#change point colour
ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass), colour = "red")

#change point colour by study area
ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass, colour = SA))

#add linear trend-line
ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_smooth(aes(x = canopy_cover, y = shrub_biomass), method = "lm") 

#trend-line by study area
ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_smooth(aes(x = canopy_cover, y = shrub_biomass, colour = SA), method = "lm") 

#with the first trend-line
ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_smooth(aes(x = canopy_cover, y = shrub_biomass, colour = SA), method = "lm") + 
  geom_smooth(aes(x = canopy_cover, y = shrub_biomass), method = "lm") # add first trend-line

#bar chart to compare biomass between areas
ggplot(data = biomass) + geom_bar(aes(x = SA, y = shrub_biomass), stat="summary", fun = "mean")

#box plot to compare biomass between areas
ggplot(data = biomass) + geom_boxplot(aes(x = SA, y = shrub_biomass))



# make publication quality figures ----------------------------------------------

ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_smooth(aes(x = canopy_cover, y = shrub_biomass, colour = SA), method = "lm") 

# add themes
ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_smooth(aes(x = canopy_cover, y = shrub_biomass, colour = SA), method = "lm") + 
  theme_bw()

ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_smooth(aes(x = canopy_cover, y = shrub_biomass, colour = SA), method = "lm") + 
  theme_dark()

ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_smooth(aes(x = canopy_cover, y = shrub_biomass, colour = SA), method = "lm") + 
  theme_minimal()

ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_smooth(aes(x = canopy_cover, y = shrub_biomass, colour = SA), method = "lm") + 
  theme_classic()

# theme components --------------------------------------------------------
#https://ggplot2.tidyverse.org/reference/theme.html

theme(
  line,
  rect,
  text,
  title,
  aspect.ratio,
  axis.title,
  axis.title.x,
  axis.title.x.top,
  axis.title.x.bottom,
  axis.title.y,
  axis.title.y.left,
  axis.title.y.right,
  axis.text,
  axis.text.x,
  axis.text.x.top,
  axis.text.x.bottom,
  axis.text.y,
  axis.text.y.left,
  axis.text.y.right,
  axis.ticks,
  axis.ticks.x,
  axis.ticks.x.top,
  axis.ticks.x.bottom,
  axis.ticks.y,
  axis.ticks.y.left,
  axis.ticks.y.right,
  axis.ticks.length,
  axis.ticks.length.x,
  axis.ticks.length.x.top,
  axis.ticks.length.x.bottom,
  axis.ticks.length.y,
  axis.ticks.length.y.left,
  axis.ticks.length.y.right,
  axis.line,
  axis.line.x,
  axis.line.x.top,
  axis.line.x.bottom,
  axis.line.y,
  axis.line.y.left,
  axis.line.y.right,
  legend.background,
  legend.margin,
  legend.spacing,
  legend.spacing.x,
  legend.spacing.y,
  legend.key,
  legend.key.size,
  legend.key.height,
  legend.key.width,
  legend.text,
  legend.text.align,
  legend.title,
  legend.title.align,
  legend.position,
  legend.direction,
  legend.justification,
  legend.box,
  legend.box.just,
  legend.box.margin,
  legend.box.background,
  legend.box.spacing,
  panel.background,
  panel.border,
  panel.spacing,
  panel.spacing.x,
  panel.spacing.y,
  panel.grid,
  panel.grid.major,
  panel.grid.minor,
  panel.grid.major.x,
  panel.grid.major.y,
  panel.grid.minor.x,
  panel.grid.minor.y,
  panel.ontop,
  plot.background,
  plot.title,
  plot.title.position,
  plot.subtitle,
  plot.caption,
  plot.caption.position,
  plot.tag,
  plot.tag.position,
  plot.margin,
  strip.background,
  strip.background.x,
  strip.background.y,
  strip.clip,
  strip.placement,
  strip.text,
  strip.text.x,
  strip.text.y,
  strip.switch.pad.grid,
  strip.switch.pad.wrap,
  ...,
  complete = FALSE,
  validate = TRUE
)


# final plot --------------------------------------------------------------

ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_smooth(aes(x = canopy_cover, y = shrub_biomass, colour = SA), method = "lm") + 
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), 
        axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black"), 
        legend.position = c(0.83, 0.83)) 

# remove white space below --- DO NOT DO THIS!! 
ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_smooth(aes(x = canopy_cover, y = shrub_biomass, colour = SA), method = "lm") + 
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black"), legend.position = c(0.83, 0.83)) +
  ylim(0,1600) #lim function actually removes the data points!!

# example why not to use the limit function
ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_smooth(aes(x = canopy_cover, y = shrub_biomass, colour = SA), method = "lm") + 
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black"), legend.position = c(0.83, 0.83)) +
  ylim(0,400) #lim function actually removes the data points!!

# remove white space below --- DO THIS 
ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_smooth(aes(x = canopy_cover, y = shrub_biomass, colour = SA), method = "lm") + 
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black"), legend.position = c(0.83, 0.83)) +
  coord_cartesian(ylim=c(0,1600)) # does not remove data points

#one option to add axis labels
ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_smooth(aes(x = canopy_cover, y = shrub_biomass, colour = SA), method = "lm") + 
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black"), legend.position = c(0.83, 0.83)) +
  coord_cartesian(ylim=c(0,1600)) + 
  xlab("Canopy cover (%)") +
  ylab("Shrub biomass (kg/ha)")
  
#my preferred method to add axis labels
ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_smooth(aes(x = canopy_cover, y = shrub_biomass, colour = SA), method = "lm") + 
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black"), legend.position = c(0.83, 0.83)) +
  coord_cartesian(ylim=c(0,1600)) + 
  scale_x_continuous("Canopy cover (%)", n.breaks =5) + #modify the number of axis ticks
  scale_y_continuous("Shrub biomass (kg/ha)", n.breaks =4, labels = scales::comma) #add comma to thousands separator

#add space between axis label and plot
ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_smooth(aes(x = canopy_cover, y = shrub_biomass, colour = SA), method = "lm") + 
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black"), legend.position = c(0.83, 0.83)) +
  coord_cartesian(ylim=c(0,1600)) + 
  scale_x_continuous("\n Canopy cover (%) \n", n.breaks =5) + # add space
  scale_y_continuous("\n Shrub biomass (kg/ha) \n", n.breaks =4, labels = scales::comma) # add space

#change legend title
ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_smooth(aes(x = canopy_cover, y = shrub_biomass, colour = SA), method = "lm") + 
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black"), legend.position = c(0.83, 0.83)) +
  coord_cartesian(ylim=c(0,1600)) + 
  scale_x_continuous("\n Canopy cover (%) \n", n.breaks =5) +
  scale_y_continuous("\n Shrub biomass (kg/ha) \n", n.breaks =4, labels = scales::comma) + 
  labs(color = "Study area")




# export your final figure ------------------------------------------------

final_plot <- ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_smooth(aes(x = canopy_cover, y = shrub_biomass, colour = SA), method = "lm") + 
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black"), legend.position = c(0.83, 0.83)) +
  coord_cartesian(ylim=c(0,1600)) + 
  scale_x_continuous("\n Canopy cover (%) \n", n.breaks =5) +
  scale_y_continuous("\n Shrub biomass (kg/ha) \n", n.breaks =4, labels = scales::comma) + 
  labs(color = "Study area")


#option 1
ggsave(filename = "final_plot1.jpeg", 
       final_plot,
       width = 7,
       height = 4.5,
       units = "in"
)

#option 2 (old school)

tiff("final_plot2.tiff", units="in", width=7, height=4.5, res=100, compression = 'lzw')

final_plot

dev.off()




# Reproducible theme -----------------------------------------------------

gg_theme <- theme(text= element_text(size=18), 
                  axis.text.x= element_text(size=16,colour = "black"), 
                  axis.text.y= element_text(size=16,colour = "black"),
                  legend.position = c(0.83, 0.83))

ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_smooth(aes(x = canopy_cover, y = shrub_biomass, colour = SA), method = "lm") + 
  theme_classic(base_family = "Times New Roman") + 
  coord_cartesian(ylim=c(0,1600)) + 
  scale_x_continuous("\n Canopy cover (%) \n", n.breaks =5) +
  scale_y_continuous("\n Shrub biomass (kg/ha) \n", n.breaks =4, labels = scales::comma) + 
  labs(color = "Study area") + gg_theme





# Multi-plot panel -------------------------------------------------------

#previous scatter plot
scatter_plot <- ggplot(data = biomass) + geom_point(aes(x = canopy_cover, y = shrub_biomass, colour = SA)) + 
  geom_smooth(aes(x = canopy_cover, y = shrub_biomass, colour = SA), method = "lm") + 
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black"), legend.position = c(0.83, 0.83)) +
  coord_cartesian(ylim=c(0,1600)) + 
  scale_x_continuous("\n Canopy cover (%) \n", n.breaks =5) +
  scale_y_continuous("\n Shrub biomass (kg/ha) \n", n.breaks =4, labels = scales::comma) + 
  labs(color = "Study area")

#previous box plot

box_plot <- ggplot(data = biomass) + geom_boxplot(aes(x = SA, y = shrub_biomass)) + 
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=18,colour = "black"),
        axis.text.y= element_text(size=16,colour = "black")) + 
  scale_y_continuous("\n Shrub biomass (kg/ha) \n", n.breaks =5, labels = scales::comma) #+ 
  xlab("") + #no x axis label 
  scale_x_discrete(label=c("\n Eastern \nOntario", "\n Western \nOntario")) #change the x tick labels


install.packages("cowplot")
library(cowplot)

plot_grid(scatter_plot, box_plot, nrow = 1, labels = "AUTO", label_size = 18, 
          label_fontfamily = "Times New Roman")

#change width of each plot
plot_grid(scatter_plot, box_plot, nrow = 1, labels = "AUTO", label_size = 18, 
          label_fontfamily = "Times New Roman", rel_widths = c(1.3,1))

multiplot <- plot_grid(scatter_plot, box_plot, nrow = 1, labels = "AUTO", label_size = 18, 
                       label_fontfamily = "Times New Roman", rel_widths = c(1.3,1))

#export multi-plot
tiff("biomass_scatter_boxplot.tiff", units="in", width=14, height=4.5, res=100, compression = 'lzw')

multiplot

dev.off()




# Plotting model outputs: Example 1 --------------------------------------------------

#data for the selection ratio plots

data<-fread("Input/selection_ratios.csv")
head(data)

# add points for coefficient estimates (or selection ratios in this case)
ggplot(data, aes(x=covariate, y=estimate, ymin = estimate-CI_down, ymax = estimate+CI_up, colour=Study_area)) +
  geom_point(position=position_dodge(width=0.55), cex=3.5) 

# add error bars / confidence intervals
ggplot(data, aes(x=covariate, y=estimate, ymin = estimate-CI_down, ymax = estimate+CI_up, colour=Study_area)) +
  geom_point(position=position_dodge(width=0.55), cex=3.5) + 
  geom_errorbar(position=position_dodge(width=0.55), linewidth=1.4) 

# make it look nice
ggplot(data, aes(x=covariate, y=estimate, ymin = estimate-CI_down, ymax = estimate+CI_up, colour=Study_area)) +
  geom_point(position=position_dodge(width=0.55), cex=3.5) + 
  geom_errorbar(position=position_dodge(width=0.55), linewidth=1.4) +
  scale_color_manual(values=c("#267300", "#cc6666", "#004da8")) + #change colour for each study area (colour theme consistent through paper)
  scale_x_discrete(limits=c("Closed", "Early Seral", "Lowlands", "LF")) + # change x axis tick labels
  xlab("") + # no x axis label
  ylab("\n Selection Ratio \n") + # change y-axis label
  theme_classic(base_family = "Times New Roman") + #theme 
  theme(text= element_text(size=18), axis.title= element_text(size=16,colour = "black"), 
        axis.text= element_text(size=16,colour = "black"), legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) + #theme specifics and change legend text size
  labs(colour = "Study area") #change legend name

# finally add a horizontal line
ggplot(data, aes(x=covariate, y=estimate, ymin = estimate-CI_down, ymax = estimate+CI_up, colour=Study_area)) +
  geom_point(position=position_dodge(width=0.55), cex=3.5) + 
  geom_errorbar(position=position_dodge(width=0.55), linewidth=1.4) +
  scale_color_manual(values=c("#267300", "#cc6666", "#004da8")) + #change colour for each study area (colour theme consistent through paper)
  scale_x_discrete(limits=c("Closed", "Early Seral", "Lowlands", "LF")) + # change x axis tick labels
  xlab("") + # no x axis label
  ylab("\n Selection Ratio \n") + # change y-axis label
  theme_classic(base_family = "Times New Roman") + #theme 
  theme(text= element_text(size=18), axis.title= element_text(size=16,colour = "black"), 
        axis.text= element_text(size=16,colour = "black"), legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) + #theme specifics and change legend text size
  labs(colour = "Study area") + #change legend name 
  geom_hline(yintercept = 1, linetype="dashed", linewidth=1.2) # add horizontal line; can use geom_vline() for vertical and geom_abline() for custom line (intercept & slope)






# Plotting model outputs: Example 2 --------------------------------------------------

# predict relationship between response variable and explanatory variables from model

#make study area a factor
biomass$SA_binary  <- as.factor(biomass$SA_binary)

# model to predict shrub biomass as a function of canopy cover, basal area, and study area (WO vs. EO)
m1 <- glm(shrub_biomass~ canopy_cover + basal_area + SA_binary, data = biomass, family = Gamma(link = "log"))
summary(m1)

##Create an empty data frame
response <- data.frame(matrix(NA, nrow = 500, ncol = 1))

##In this example, we will simulate the effects of canopy cover on shrub biomass
##Create some data
random_canopy_cover <- floor(runif(500, min = 0, max = 100))

##the 'floor' part of the argument makes it only integers
##generate 500 random values of canopy cover between 0 and 100

##Populate data frame with the data
response <- cbind(response, random_canopy_cover)

##Find mean value for the basal area variable
##Will have to run the model separately for WO and EO
##since it is a factor
mean(biomass$basal_area)

##Add canopy closure and ecosite to the response data frame
response$basal_area <- 17.73401
response$SA_binary <- 0 # EO

##Clean up data frame
response_EO <- response %>% 
  dplyr::select(c(2:4)) %>% 
  dplyr::rename(canopy_cover = random_canopy_cover) %>% 
  dplyr::mutate(SA_binary = as.factor(SA_binary)) %>% 
  dplyr::mutate(canopy_cover = as.integer(canopy_cover))

##Use predict function to generate shrub biomass estimates
response_EO$predict_shrub_biomass <- 0
response_EO$SE <- 0

for (i in 1:nrow(response_EO)) {
  n <- predict.glm(m1, newdata = response_EO, type = "response", se.fit = TRUE)
  response_EO$predict_shrub_biomass <- n$fit
  response_EO$SE <- n$se.fit
}


##Repeat the process for WO
##Create an empty data frame
response <- data.frame(matrix(NA, nrow = 500, ncol = 1))

##Create random canopy cover data
random_canopy_cover <- floor(runif(500, min = 0, max = 100))

##Populate data frame with random numbers
response <- cbind(response, random_canopy_cover)

##Add canopy closure and ecosite to the response data frame
response$basal_area <- 17.73401
response$SA_binary <- 1 # WO

##Clean up data frame
response_WO <- response %>% 
  dplyr::select(c(2:4)) %>% 
  dplyr::rename(canopy_cover = random_canopy_cover) %>% 
  dplyr::mutate(SA_binary = as.factor(SA_binary)) %>% 
  dplyr::mutate(canopy_cover = as.integer(canopy_cover))

##Use predict function to generate shrub biomass estimates
response_WO$predict_shrub_biomass <- 0
response_WO$SE <- 0

for (i in 1:nrow(response_WO)) {
  n <- predict.glm(m1, newdata = response_WO, type = "response", se.fit = TRUE)
  response_WO$predict_shrub_biomass <- n$fit
  response_WO$SE <- n$se.fit
}


##Combine the two datasets
response_EO_WO <- rbind(response_EO, response_WO)
head(response_EO_WO)

##Graph the predicted biomass
ggplot(aes(x = canopy_cover, y = predict_shrub_biomass, color = SA_binary), data = response_EO_WO) +
  geom_ribbon(aes(ymin = predict_shrub_biomass - SE, ymax = predict_shrub_biomass + SE), alpha = 0.2) + #ribbon adds the confidence band
  geom_smooth() +
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black")) +
  scale_x_continuous("\n Canopy cover (%) \n", n.breaks =5) +
  scale_y_continuous("\n Predicted shrub biomass (kg/ha) \n", n.breaks =4, labels = scales::comma)

# change legend labels
ggplot(aes(x = canopy_cover, y = predict_shrub_biomass, color = SA_binary), data = response_EO_WO) +
  geom_ribbon(aes(ymin = predict_shrub_biomass - SE, ymax = predict_shrub_biomass + SE), alpha = 0.2) + #ribbon adds the confidence band
  geom_smooth() +
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black")) +
  scale_x_continuous("\n Canopy cover (%) \n", n.breaks =5) +
  scale_y_continuous("\n Predicted shrub biomass (kg/ha) \n", n.breaks =4, labels = scales::comma) +
  labs(colour = "Study area") + # change legend title
  scale_colour_discrete(labels=c('EO', 'WO')) #change legend values




# Phil's tips and tricks --------------------------------------------------

#superscript and Unicode symbols

ggplot(data = biomass) + geom_point(aes(x = basal_area, y = shrub_biomass), cex = 2) + 
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black"), legend.position = c(0.83, 0.83)) +
  coord_cartesian(ylim=c(0,1600)) + 
  scale_x_continuous("\n Basal area (m^2/ha) \n", n.breaks =5) +
  scale_y_continuous("\n Shrub biomass (kg/ha) \n", n.breaks =4, labels = scales::comma) 

# add superscript using bquote
ggplot(data = biomass) + geom_point(aes(x = basal_area, y = shrub_biomass), cex = 2) + 
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black"), legend.position = c(0.83, 0.83)) +
  coord_cartesian(ylim=c(0,1600)) + 
  scale_x_continuous(bquote("Basal area " (m^2/ha)), n.breaks =5) + #using bquote
  scale_y_continuous("\n Shrub biomass (kg/ha) \n", n.breaks =4, labels = scales::comma) 

  
# add superscript using Unicode
ggplot(data = biomass) + geom_point(aes(x = basal_area, y = shrub_biomass), cex = 2) + 
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black"), legend.position = c(0.83, 0.83)) +
  coord_cartesian(ylim=c(0,1600)) + 
  scale_x_continuous("\n Basal area (m\u00B2/ha) \n", n.breaks =5) + #m/u00B2 is Unicode for ^2
  scale_y_continuous("\n Shrub biomass (kg/ha) \n", n.breaks =4, labels = scales::comma) 


# add delta symbol using Unicode
ggplot(data = biomass) + geom_point(aes(x = basal_area, y = shrub_biomass), cex = 2) + 
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black"), legend.position = c(0.83, 0.83)) +
  coord_cartesian(ylim=c(0,1600)) + 
  scale_x_continuous("\n Basal area (m\u00B2/ha) \n", n.breaks =5) + #\u00B2 is Unicode for ^2
  scale_y_continuous("\n \u0394AIC \n", n.breaks =5) #\u0394 is Unicode for delta symbol


#add non-linear trend-line
#logistic power equation relating shrub biomass to basal area
#y = a/(1+(x/b)^c)
#y = 479.11/(1+(x/7.51)^1.18)

ggplot(data = biomass) + geom_point(aes(x = basal_area, y = shrub_biomass), cex = 2) + 
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black"), legend.position = c(0.83, 0.83)) +
  coord_cartesian(ylim=c(0,1600)) + 
  scale_x_continuous("\n Basal area (m\u00B2/ha) \n", n.breaks =5) + #m/u00B2 is Unicode for ^2
  scale_y_continuous("\n Shrub biomass (kg/ha) \n", n.breaks =4, labels = scales::comma) + 
  labs(color = "Study area") + 
  geom_function(fun = function(x) 479.11/(1+(x/7.51)^1.18), colour ="red", data = biomass, 
                aes(x = basal_area, y=shrub_biomass), cex =1.3) 


#label points

ggplot(data = biomass) + geom_point(aes(x = basal_area, y = shrub_biomass), cex = 2) + 
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black"), legend.position = c(0.83, 0.83)) +
  coord_cartesian(ylim=c(0,1600)) + 
  scale_x_continuous("\n Basal area (m\u00B2/ha) \n", n.breaks =5) + #m/u00B2 is Unicode for ^2
  scale_y_continuous("\n Shrub biomass (kg/ha) \n", n.breaks =4, labels = scales::comma) + 
  labs(color = "Study area") + 
  geom_text(aes(x = basal_area, y = shrub_biomass, label=Site), size=3, hjust=-0.1, vjust=0)



# plotting date - using Julian or ordinal day

#Julian day
ggplot(data = biomass) + geom_point(aes(x = Julian_day, y = shrub_biomass), cex = 2) + 
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black"), legend.position = c(0.83, 0.83)) +
  coord_cartesian(ylim=c(0,1600)) + 
  scale_x_continuous("\n Julian day \n", n.breaks =6) + #use Julian day to plot, but change the labels
  scale_y_continuous("\n Shrub biomass (kg/ha) \n", n.breaks =4, labels = scales::comma)

#changing the labels to dates                     
ggplot(data = biomass) + geom_point(aes(x = Julian_day, y = shrub_biomass), cex = 2) + 
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black"), legend.position = c(0.83, 0.83)) +
  coord_cartesian(ylim=c(0,1600)) + 
  scale_x_continuous("\n Date \n", breaks=c(166,196,227,258,288), 
                     labels=c("15 Jun", "15 Jul", "15 Aug", "15 Sept", "15 Oct")) + #use Julian day to plot, but change labels
  scale_y_continuous("\n Shrub biomass (kg/ha) \n", n.breaks =4, labels = scales::comma)

#change x-extent
ggplot(data = biomass) + geom_point(aes(x = Julian_day, y = shrub_biomass), cex = 2) + 
  theme_classic(base_family = "Times New Roman") + 
  theme(text= element_text(size=18), axis.text.x= element_text(size=16,colour = "black"), 
        axis.text.y= element_text(size=16,colour = "black"), legend.position = c(0.83, 0.83)) +
  coord_cartesian(ylim=c(0,1600)) + 
  scale_x_continuous("\n Date \n", breaks=c(166,196,227,258,288), 
                     labels=c("15 Jun", "15 Jul", "15 Aug", "15 Sept", "15 Oct")) +
  scale_y_continuous("\n Shrub biomass (kg/ha) \n", n.breaks =4, labels = scales::comma) + 
  coord_cartesian(xlim=c(155,290))




