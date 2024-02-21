
install.packages('tidyverse') # for data manipulation and visualization
install.packages('rstatix') # for data manipulation and visualization
library(tidyverse)
library(rstatix)
setwd("S:/Employee Shared Folders/ewells/BDMA infrastructure/Training/R master class 2021")
zoop_density<-read_csv("zoop_density_example.csv")
#format the site and habitat columns
zoopden<-zoop_density %>% #specifies the data set
  mutate_at(vars(region,habitat), factor) #converts the two columns from character to factor
glimpse(zoopden)
(zoop_summary<-zoopden %>% #name of the data set
    group_by(region, habitat) %>% #columns to group by
    summarize(n = n())
)
#boxplot: zooplankton density grouped by region and habitat type 
(plot_reg_hab<-ggplot(data=zoopden #specifies the data set
                      , aes(x = habitat, y = zoop_density)) + #sets the x and y variables for the plots
    geom_boxplot()+  #tells ggplot that we want a boxplot
    facet_wrap(~region) #splits the data into four panels based on region
)
zoopden %>% #specifies the data set
  group_by(habitat) %>% #groups the data by habitat type
  identify_outliers(zoop_density) #looks for outliers in the zooplankton density data
zoopden_sub<-zoopden %>% #specifies the data set
  filter(zoop_density<0.05) #removes any value over 0.05 individuals per mL
(plot_hab_sub<-ggplot(data=zoopden_sub, aes(x = habitat, y = zoop_density)) + 
    geom_boxplot()
)
zoopden_sub %>% #specifies the data set
  group_by(habitat) %>% #groups the data by habitat type
  shapiro_test(zoop_density) #runs the Shapiro-Wilks normality test
(plot_hist <- ggplot(zoopden_sub, aes(x = zoop_density))+
    geom_histogram()+
    facet_wrap(~habitat)
)
(plot_hab_qq <- ggplot(zoopden_sub, aes(sample = zoop_density))+
    stat_qq() + stat_qq_line()+
    facet_wrap(~habitat)
)
zoopden_sub %>% #specifies the data set
  levene_test(zoop_density~habitat) #conducts the test
t.test(zoop_density ~ habitat, data = zoopden_sub, var.equal=T)
zoopden_ns<-zoopden_sub %>% 
  filter(region == "North" | region == "South")
(plot_ns<-ggplot(data=zoopden_ns, aes(x = region, y = zoop_density)) + 
    geom_boxplot()
)
zoopden_nsw<-zoopden_sub %>% 
  filter(region == "North" | region == "South" | region =="West")
(plot_nsw<-ggplot(data=zoopden_nsw, aes(x = region, y = zoop_density)) + 
    geom_boxplot()
)
zoopden_nsw %>%
  group_by(region) %>%
  shapiro_test(zoop_density)
zoopden_nsw %>% #specifies the data set
  levene_test(zoop_density~region) #conducts the test
mod_three_reg<-aov(zoop_density~region, data=zoopden_nsw)
summary(mod_three_reg) 
plot(mod_three_reg)
TukeyHSD(mod_three_reg)


