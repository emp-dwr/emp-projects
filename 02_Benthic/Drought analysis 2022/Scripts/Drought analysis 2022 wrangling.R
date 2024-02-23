## Playing with data to look at patterns in droughts.


library(tidyverse)
library (dplyr)
library(readxl)
library(readr)
library(data.table) ## allows fwrite function
getwd()

##################  Data inputs


##load raw data input; note that everything except organism code is saved as text here 
benraw = read_excel('Data/Benthic data 1975-2021 for EDI input.xlsx', 
                    sheet = "75-21 data", 
                    col_types = c("date", "text", "text", "text",  "text", "numeric", "numeric", "text"))
head(benraw)
str(benraw)

## OK, let's read in some taxonomic info from the benthic organism keylist
orgs = read_excel('Data/Benthic organism keylist 2021.xlsx', 
                  sheet = "Benthic organism keylist 2021", 
                  col_types = c("text", "text", "text", "text", "text", "text", "text", "text"))
head(orgs)

stats = read_excel('Data/Benthic_Stations_for_EDI.xlsx', 
                   sheet = "Benthic_Stations", 
                   col_types = c("numeric", "text", "text", "numeric", "numeric", "text", "text", "text", "text"))
head(stats)

WY = read_excel('Data/Water Year Hydrologic Classification indices, 1901-2021.xlsx', 
                   sheet = "Sheet1", 
                   col_types = c("numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "text"))
head(WY)

##################  Add 0s to spp data by making wide then long

## make original data wide
benwide = benraw %>% #specify the data set you want to pivot
  pivot_wider(names_from = OrganismCode, #give the name of the new column you want to use for the names of the new columns
              values_from = Count, #name of the column for the values
              values_fill = 0 # replace NA with 0
  )

## id_cols = c(SampleDate, StationCode,Lab_Sample_Number, Location, Grab, Year, Month),

head(benwide) #look for first organism column.
str(benwide)
benwide[,ncol(benwide)]  #find name of last column for pivoting back to long

## Now put it back to long.  Super-long, including all zeros.
bendata = benwide %>% #specify the data set you want to pivot
  pivot_longer(cols = "5750":"5989", #specify the columns to pivot
               names_to = "OrganismCode", #give the name of the new column you want to create
               values_to = "Count" #name of the column for the values
  )
head(bendata)
str(bendata)

## Just get Potamocorbula data,1980 and onward

bendata1 = bendata %>%
  filter(OrganismCode == "6890"& Year>1979)
str(bendata1)

## convert to CPUE, average for station*year, take out zeros

bendata2 <- mutate(bendata1, CPUE = (Count/0.052))%>%
  group_by(Year, StationCode, OrganismCode) %>%
  summarise(MeanCPUE = mean(CPUE), )%>%
  filter(MeanCPUE>0)  
str(bendata2)

## Add Water year type

bendata3 <- left_join(x = bendata2, y = WY,  #left_join keeps all rows of x, matches w/ y
                        by = c("Year"))%>% #note, could join using 2 or more criteria; don't need "by" argument
  select(1:4,8:9, 15:16)
str(bendata3)





###################### Graphs

## Overall theme for charts - Horizontal lines only, centered title
clamtheme <- theme(panel.background = element_rect(fill = "white", color ="black", size=1),
                    panel.grid.major.y = element_line(colour="grey", size = (0.2)),
                    panel.grid.major.x = element_blank(),
                    plot.title = element_text(hjust = 0.5))

base <- ggplot(bendata3, aes(Year, MeanCPUE, color=Sac_Index)) + geom_point() +
  labs(color = "Sacramento Valley Index") + scale_y_continuous(trans='log10')

base2 <- ggplot(bendata3, aes(Sac_Index , MeanCPUE, color=Sac_Index)) + geom_point() +
  labs(color = "Sacramento Valley Index") + scale_y_continuous(trans='log10')

base3 <- ggplot(bendata3, aes(OneYr_Lag_Sac_Index, MeanCPUE, color=Sac_Index)) + geom_point() +
  labs(color = "One Year Lag Sacramento Valley Index") + scale_y_continuous(trans='log10')

base + clamtheme + ggtitle("Potamocorbula amurensis by year") +
  scale_colour_gradient(low = "yellow", high = "blue") +
  scale_size(guide = 'none') +
  facet_wrap(~StationCode,ncol=3)

base2 + clamtheme + ggtitle("Potamocorbula amurensis by Sacramento Valley Index") +
  scale_colour_gradient(low = "yellow", high = "blue") +
  scale_size(guide = 'none') +
  facet_wrap(~StationCode,ncol=3)

base2 + clamtheme + ggtitle("Potamocorbula amurensis by Sacramento Valley Index") +
  scale_colour_gradient(low = "yellow", high = "blue") +
  scale_size(guide = 'none') 

base3 + clamtheme + ggtitle("Potamocorbula amurensis by One Year Lag Sacramento Valley Index") +
  scale_colour_gradient(low = "yellow", high = "blue") +
  scale_size(guide = 'none') +
  facet_wrap(~StationCode,ncol=3)
  
base3 + clamtheme + ggtitle("Potamocorbula amurensis by One Year Lag Sacramento Valley Index") +
  scale_colour_gradient(low = "yellow", high = "blue") +
  scale_size(guide = 'none') 