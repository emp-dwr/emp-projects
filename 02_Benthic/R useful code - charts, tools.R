
##Data wrangling
###################################################
# Loading data

# if working in a project, sometimes get an error message about not seeing the file path if I don't run this
list.files(path = 'Data/', pattern = "*.xlsx", full.names = TRUE)

###################################
# Convert some columns to factors

biomass2 =  biomass1 %>%
  mutate(across(c(1,3,11,12), .fns=as.factor))  # make some columns factors

###################################

#Turning occurrence data into wide or long with all the zeros


##load raw data input which only lists occurrences; note that organism code is saved as text here 
## also note that data has individual ID numbers for each row, which helps prevent duplicate row errors
benraw = read_excel('Data/Benthic data 1975-2020 for EDI input.xlsx', 
                    sheet = "75-20 data", 
                    col_types = c("numeric", "text", "date", "text", "text", "text", "numeric", "text", "numeric", "text", "numeric", "text"))

## make original data wide
benwide = benraw%>% #specify the data set you want to pivot
  pivot_wider(names_from = OrganismCode, #give the name of the new column you want to use for the names of the new columns
              values_from = Count, #name of the column for the values
              values_fill = 0 # replace NA with 0
  )

benwide[,ncol(benwide)]  #find name of last column for pivoting back to long


## Now put it back to long.  Super-long, including all zeros.
bendata = benwide %>% #specify the data set you want to pivot
  pivot_longer(cols = "5750":"6526", #specify the columns to pivot - these are organism codes
               names_to = "OrganismCode", #give the name of the new column you want to create
               values_to = "Count" #name of the column for the values
  )

############################
#Add a column from a lookup table

#Left-join benthic taxonomy dataset (bendataorg) with station data (stats), by a defined name, and only select the columns of the final combined dataset (bendatafinal) that we want to keep.  need dplyr
bendatafinal <- bendataorg %>%
  left_join(stats, by = "StationCode")%>% # could write left_join("lookupdf", by = "x_name", "yname") if the columns were named soemthing differently
  select(1:17,20,21)
  

##Writing a CSV super-fast
fwrite(bendatafinal, "C:/Users/ewells/Desktop/DWR Benthic data 1975-2020.csv", row.names=TRUE)

#########################################
## CHART examples



## Overall theme for charts - Horizontal lines only, centered title
clamtheme2 <- theme(panel.background = element_rect(fill = "white", color ="black", size=1),
                    panel.grid.major.y = element_line(colour="grey", size = (0.2)),
                    panel.grid.major.x = element_blank(),
                    plot.title = element_text(hjust = 0.5))


## histogram
hist_depth<-ggplot(data =clams, aes(x = depth)) + geom_histogram(fill = "dodgerblue") 

hist_depth +clamtheme2 +ggtitle("Water Depth")+
  labs(x="Water depth (ft)")

##Stacked bar plots - this counts actual data instances/rows; would use geom_col for a table with other numeric data already done
c_zeros <- ggplot(data=clams, aes(x=month)) + geom_bar(aes(fill=c_round_YN, color=c_round_YN))
c_zeros + clamtheme3 +  labs(y="Count", x = "Month", fill = "Corbicula") +
  scale_fill_manual(name = "Corbicula", labels = c("Present", "Absent"), values = c("gray", "white"))+
  scale_color_manual(values = c("#999999", "#999999"))+
  guides(fill = "legend", color = "none")


##scatterplot, coded by a continuous variable for color
c_afdm_depth2 <- ggplot(clams, aes(depth, c_round_YN, color=chlor, size = (5))) + geom_point() +
  labs(y="Ash-free dry mass (g/m2)", x = "Water temperature (C)", color = "Chlorophyll")

c_afdm_depth2 + clamtheme + ggtitle("Corbicula amurensis AFDM by water temperature") +
  scale_colour_gradient(low = "darkolivegreen1", high = "chartreuse4")+
  scale_y_discrete(breaks = 0:1, labels=c("Zero AFDM","AFDM present"))+  ## want to use new labels for categories
  scale_size(guide = 'none')




## boxplot
plot_depth<-ggplot(data=clams, aes(x = habitat, y = depth, fill=habitat)) + geom_boxplot() 

plot_depth + clamtheme2 + ggtitle("Water depth by habitat")+
  labs(y="Water depth (ft)", x = "Habitat") +
  scale_fill_manual(values = c("blue", "green", "brown"))

## dotplot
## Corbicula AFDM Y/N by depth binary dotplot - note that have to construct with flipped axes
C_YN_depth_dot<-ggplot(clams, aes(y = depth, x = c_round_YN, fill = c_round_YN, color=c_round_YN)) +  geom_dotplot(binaxis = "y", stackdir = "center")

C_YN_depth_dot + clamtheme + labs(x="Corbicula fluminea", y = "Water depth (m)") + coord_flip() +
  scale_x_discrete(breaks = 0:1, labels=c("Absent","Present"))+
  scale_fill_manual(values = c("dodgerblue2", "white"))+
  scale_color_manual(values = c("dodgerblue2", "dodgerblue2"))+
  guides(fill = "none", color = "none")

## violinplot
C_YN_depth<-ggplot(data=clams, aes(x = depth, y = c_round_YN, fill = c_round_YN)) + 
  geom_violin(trim=FALSE)

C_YN_depth + clamtheme + ggtitle("Corbicula amurensis by depth")+
  labs(y="Corbicula amurensis", x = "Water depth (ft)") +
  scale_y_discrete(breaks = 0:1, labels=c("Absent","Present"))+
  scale_fill_manual(values = c("#FFFFFF", "dodgerblue2"))+
  scale_color_manual(values = c("#dodgerblue2", "#dodgerblue2"))+
  guides(fill = "none", color = "none")


## Violinplot, each category further split by second category
C_YN_depth<-ggplot(data=clams, aes(x = w_temp, y = c_round_YN, fill=month, color=month)) + 
geom_violin(trim=FALSE)

C_YN_depth + clamtheme2 + ggtitle("Corbicula amurensis presence/absence by temperature")+
labs(y="Corbicula AFDM", x = "Water temperature (C)", fill = "Month") +
  scale_y_discrete(breaks = 0:1, labels=c("Zero AFDM","AFDM present"))+
  scale_fill_manual(values = c("#999999", "#E69F00"))+
  scale_color_manual(values = c("#999999", "#E69F00"))+
  guides(fill = guide_legend(reverse=TRUE), color = "none")