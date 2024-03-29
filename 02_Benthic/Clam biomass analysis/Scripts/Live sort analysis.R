#### Analyzing live sort data!

## first, some data

biomass_in = read_excel('Data/Compiled live sort data, 2006-2021.xlsx', 
                        sheet = "formatted for R", 
                        col_types = c("text", "text","date", "text", "numeric", "numeric", "numeric","numeric","numeric","numeric","numeric","text", "text",  "text", "text", "text",  "text", "text", "text")) 
# Input columns = tray,	station,	date,	species,	no_orgs,	size_class,	pan_mass,	dry_mass_incl_pan,	afdm_incl_pan,	total_afdm,	afdm_per_org,	comments,	Total animal weight,	Average weight per individual,	QC_flag,	QC_exclude,	Check_datasheets,	Year,	Month
str(biomass_in) #summarizes data in console

##########################
# Step 2
##########################

## Create length:biomass regresssion and extract slopes and intercepts for each species*month*site combination, and extract coefficients


##Wrangling biomass into better shape:

biomass1 = biomass_in %>% 
  select(2:6,10:12,15:16,18:19) #only keeping wanted columns 
str(biomass1)
biomass<-subset(biomass1, is.na(biomass2$"QC_exclude"))   ##Remove "QC_exclude" lines that =y; we only keep those that were blanks in original data and loaded as NA here. 
str(biomass)

## create new variables for loglength:logbiomass regression

biomass$log_afdm<-log10(biomass$afdm_per_org) # some values will be NAs or infinites; because they're negative; omit them in the further analysis 

######!!!!!!!!!!!!!!!!!!!!! Figure this out before doing the below ##################




biomass$log_length<-log10((biomass$size_class)+0.5)  #add 0.5 to make it the middle of the size bin
str(biomass)
view(biomass)
# levels(biomass$species)

modelmass = biomass %>%  #new DF 
  filter(size_class > 0) ## Remove all size zero clams since they tend to throw off the data (this also removes everything in the "No clams" level of species)

## Creating new dataset that summarises the regression coefficients for each station*species*month combination


modelmass2 <- modelmass%>%
  group_by(station, month, year, species) %>% 
  filter(n()>4)%>%  #this removes groups with less than 5 rows, since those are not creating good regressions anyway
  summarise(number = n(),
            intercept= summary(lm(log_afdm ~ log_length, na.action=na.omit))$coefficients["(Intercept)", "Estimate"], # what coefficient to pull out
            slope= summary(lm(log_afdm ~ log_length, na.action=na.omit))$coefficients["log_length", "Estimate"])


fwrite(modelmass2,"Products/Clam length-biomass regressions, 2007-2020.csv", row.names=FALSE)   