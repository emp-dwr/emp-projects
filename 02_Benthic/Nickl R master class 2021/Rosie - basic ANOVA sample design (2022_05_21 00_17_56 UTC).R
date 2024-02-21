#Sampling Design Example Code
#Rosemary Hartman 4/8/2021

#load libraries for data manipulation
library(readxl)
library(tidyverse)


#load data collected using three sites for analysis
threesites = read_excel("testdata.xlsx", sheet = "Threesites")

#take a quick look at the data
View(threesites)

#summarize the data and calculate mean, Standard Deviation, and Standard Error)
sum1 = summarise(group_by(threesites, SiteType), 
                 mCPUE = mean(CPUE), sdCPUE = sd(CPUE), seCPUE = sdCPUE/length(CPUE))

#quick plot
p1 = ggplot(sum1, aes(x= SiteType, y = mCPUE)) + #define your data set, x and y variables
  geom_bar(stat = "identity") + #it's a bar plot that is plotting the actual values
  geom_errorbar(aes(ymin = mCPUE-seCPUE, ymax = mCPUE +seCPUE)) #add error bars
p1
#note the teeny tiny error bars

#quick ANOVA
a1 = aov(CPUE~ SiteType, data = threesites)
summary(a1)

#pairwise comparisons using a Tukey post-hoc
TukeyHSD(a1)

#now let's do it with site as the experimental unit
twelveesites = read_excel("testdata.xlsx", sheet = "twelevesites")

View(twelveesites)

#Summarise it.  
sum2 = summarise(group_by(twelveesites, SiteType), 
                 mCPUE = mean(CPUE), sdCPUE = sd(CPUE), seCPUE = sdCPUE/length(CPUE))
sum2

#But wait! Standard error (Standard deviation divided by number of replicates)
#should be calculated using the number of true replicates, not the total number of observations.
#Let's first calculate the mean for the site, then get the mean and SD by site type.

#first teh mean by site
sum3 = summarize(group_by(twelveesites, Site, SiteType), 
                 mCPUE = mean(CPUE))

#now the mean by site type, with the standard error by site
sum4 = summarise(group_by(sum3, SiteType), mCPUE2 = mean(mCPUE), sdCPUE = sd(mCPUE),
                 seCPUE = sdCPUE/length(mCPUE))

#plot it again
p2 = ggplot(sum4, aes(x= SiteType, y = mCPUE2)) + geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mCPUE2-seCPUE, ymax = mCPUE2 +seCPUE))
p2

#compare the two plots
p1
p2

#Now let's do an ANOVA
a2 = aov(CPUE~ SiteType, data = twelveesites)
summary(a2)
TukeyHSD(a2)

#But what about those repeated measures?
# Using "Error = site" designates the fact that the site is our experimental unit. 
a3 = aov(CPUE ~ SiteType + Error(Site), data = twelveesites)
summary(a3)
#note the difference in degrees of freedom between the two models (a2 versus a3)


#The original data set can't be used to tell the difference between site type, but
#it can be used to detect differences between sites!

a4 = aov(CPUE ~ Site, data = threesites)
summary(a4)

#Note that this assumes the four samples per month were randomly selected.
#if they were fixed, you should include the random effect of subsite. But that's a 
#story for another day. 