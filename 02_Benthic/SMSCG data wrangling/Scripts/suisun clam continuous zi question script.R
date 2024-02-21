## Data here is from 2018-2021 Suisun Marsh clam monitoring by DWR, headed by Betsy Wells, with heavy lifting of stats by Rosemary Hartman.

## dataflow is: 

## 1a) input data: the EDI file, with 42 fields, including biomass, grazing rates, totals, and WQ and sample data.  Rename for easier analysis.

## 1b) OR, if not 1a, Recreate "SMSCG data wrangling" workflow:
## 1b1) Input tables: station list, field data, clam measuring data, live sort data, filtration rates 
## 1b2) Create length:biomass regresssion and extract slopes and intercepts for each species*month*site combination, and extract coefficients
## 1b3) Add regression coefficients (step 2) to clam measuring data to get biomass per size class
## 1b4) Add temperature, filtration, and density to get filtration and grazing per size class
## 1b5) Create a) summarized table of biomasses, grazing, and filtration rate, w/ species (Corbicula, Potamocorbula, and totals) turned into wide format ready for R or GIS analysis, and b) create rounded or grouped variables for analysis (this is the EDI file)

## 2) Explore biomass/grazing data to look at structure, try to find what distributions are best for data
## 3) Do models to see what factors best predict clam biomass/grazing.  
## 4) Make graphs

################################
## 0: Libraries
################################
library(tidyverse)
library(readxl)
library(visreg)  #additional library to plot data
# library(MASS)
# library(pscl) #The "zeroinfl" function in the pscl package can do zero inflated negative binomial or poisson models.
library(emmeans) # allows post-hoc means comparisons
library(MuMIn) #load library to do dredge model function
library(car) # to do multiple collinearity checks
library(glmmTMB) # glm fitting package - using this instead of pscl
library(DHARMa) # checking of dispersion and residuals
library(effects) # look at conditional effects, to determine detransformed means/effect sizes
library(broom) #tidies up data outputs from glms and adds columns to original data
library(ggeffects) # plotting conditional effects plots
library(mosaic) ## function "zscore" allows normalizing of continuous variables

################################
## 1a: Data input: if you've already created a data file, like the EDI file, with all the fields necessary
################################
## station list
clam_in <- read.csv('Products/SMSCG_clam_EDI_2018_2021.csv')
str(clam_in)

## rename fields for easier analysis

clam_in = 
  rename(clam_in,
         year	=	Year,
         month	=	Month,
         station	=	Station,
         station_alias	=	Station_alias,
         date	=	Date,
         north	=	North_decimal_degrees,
         west	=	West_decimal_degrees,
         c_afdm	=	Corbicula_AFDM_g_per_m2,
         c_density	=	Corbicula_density_per_m2,
         c_filt	=	Corbicula_filtration_rate_m3_per_m2_per_day,
         c_graze	=	Corbicula_grazing_rate_m3_per_m2_per_day,
         p_afdm	=	Potamocorbula_AFDM_g_per_m2,
         p_density	=	Potamocorbula_density_per_m2,
         p_filt	=	Potamocorbula_filtration_rate_m3_per_m2_per_day,
         p_graze	=	Potamocorbula_grazing_rate_m3_per_m2_per_day,
         t_afdm	=	Total_AFDM_g_per_m2,
         t_density	=	Total_density_per_m2,
         t_filt	=	Total_filtration_rate_m3_per_m2_per_day,
         t_graze	=	Total_grazing_rate_m3_per_m2_per_day,
         c_turn	=	Corbicula_grazing_turnover_per_day,
         p_turn	=	Potamocorbula_grazing_turnover_per_day,
         t_turn	=	Total_grazing_turnover_per_day,
         depth_ft	=	Depth_ft,
         depth	=	Depth_m,
         orgmatter	=	Percent_Organic_Matter,
         clay	=	Percent_Clay,
         silt	=	Percent_Silt,
         mica	=	Percent_Mica,
         fine_sand	=	Percent_Fine_Sand,
         coarse_sand	=	Percent_Coarse_Sand,
         gravel	=	Percent_Gravel,
         fines	=	Percent_Fines,
         w_temp	=	Water_temperature_C,
         salinity	=	Specific_conductivity_uS_per_cm,
         pH	=	pH,
         chlor	=	Chlorophyll_a_ug_per_L,
         turb	=	Turbidity_NTU,
         DO	=	Dissolved_oxygen_mg_per_L,
         sediment	=	Sediment,
         habitat	=	Habitat,
         habitat_type	=	Habitat_type,
         both_years	=	Done_all_years
  )

str(clam_in)
         
clams<-clam_in %>%
  filter(both_years=="yes")#subset of only sites done all years
 
clams<- clams%>%
  mutate(
      station <- factor(clams$station),
        salinity2 = zscore(clams$salinity),  #putting continuous variables on one scale
        chlor2 = zscore(clams$chlor), 
      turb2 = zscore(clams$turb),
      depth2 = zscore(clams$depth),
      w_temp2 = zscore(clams$w_temp),
      DO2 = zscore(clams$DO),
      c_afdm_mg = c_afdm*1000 , #putting RVs into mg/m2 and L/day
      p_afdm_mg = p_afdm*1000 ,
      t_graze_L = t_graze*1000,
      t_filt_L = t_filt*1000,
      t_filt_L = t_filt*1000,
      t_afdm_mg=t_afdm*1000,
      c_afdm_mg_r=round(c_afdm_mg), #rounding RVs into integers to treat as counts
      p_afdm_mg_r=round(p_afdm_mg),
      t_filt_L_r=round(t_filt_L),
      t_graze_L_r=round(t_graze_L),
      t_afdm_mg_r=round(t_afdm_mg),
      c_round_YN = as.logical(c_afdm_mg_r),
      p_round_YN = as.logical(p_afdm_mg_r),
      t_filt_YN = as.logical(t_filt_L_r),
      t_round_YN = as.logical(t_graze_L_r),
      t_AFDM_YN = as.logical(t_afdm_mg_r),
      p_afdm_mg_log=log(p_afdm_mg+1)# pre-transforming by log  
  )
clams$year <-factor(clams$year,levels = c("2018", "2019", "2020", "2021"))
clams$habitat <- factor(clams$habitat,levels = c("River", "Channel", "Slough"))
clams$month <- factor(clams$month,levels = c("July", "September")) 
clams$sediment <- factor(clams$sediment,levels = c("Fines", "Sand", "Gravel", "Organic"))

str(clams)

names(which(colSums(is.na(clams))>0))## where are NAs? Looks like only of estimated sediment makeup (clay, silt, etc.)
summary(clams) ## take a squint at some of the scales of the continuous variables

################################
## 2:  EXPLORING
################################

## exploratory plots!
#first some histograms
ggplot(clams) + geom_histogram(aes(x = c_afdm_mg))
ggplot(clams) + geom_histogram(aes(x = p_afdm_mg))
ggplot(clams) + geom_histogram(aes(x = t_graze_L))
#So, you're definitely not normally distributed!

#will log-transforming help?  
ggplot(clams) + geom_histogram(aes(x = log(c_afdm_mg+1)))
ggplot(clams) + geom_histogram(aes(x = log(p_afdm_mg+1)))
ggplot(clams) + geom_histogram(aes(x = log(t_graze_L+1)))

#Gross. Even log-transformed those are bad. Highly zero-inflated. 
# But!  P and C have now developed little normal curves to the right, plus the zeros.  so maybe zero-inflated negative binomials will work.  Or just NB models.

## is it overdispersed?  Yup, variance way different than mean; variance much bigger than mean 
var(clams$c_afdm_mg)
mean(clams$c_afdm_mg)
var(clams$p_afdm_mg)
mean(clams$p_afdm_mg)
var(clams$t_graze_L)
mean(clams$t_graze_L)

## What if we did a dispersion test to see if it's a poisson distribution.  And no, none of them are; all probabilities =0.

dispersion_test <- function(x) ## this was written by Rosie Hartmann
{
  res <- 1-2 * abs((1 - pchisq((sum((x - mean(x))^2)/mean(x)), length(x) - 1))-0.5)
  
  cat("Dispersion test of count data:\n",
      length(x), " data points.\n",
      "Mean: ",mean(x),"\n",
      "Variance: ",var(x),"\n",
      "Probability of being drawn from Poisson distribution: ", 
      round(res, 3),"\n", sep = "")
  
  invisible(res)
}

dispersion_test(clams$c_afdm_mg)
dispersion_test(clams$p_afdm_mg)
dispersion_test(clams$t_graze_L)

################################
## 3:  MODELS
################################

##POTAMOCORBULA

## look at the data
## Right, so, log-transforming gives a somewhat better distribution has appeared to the right, in addition to the zeros!
ggplot(clams) + geom_histogram(aes(x = p_afdm_mg))
# That's a whole lot of zeros!
ggplot(clams) + geom_histogram(aes(x = log(p_afdm_mg+1)))


## check distributions as a log transformation
mp1 = lm(log(p_afdm_mg+1)~habitat+ month + sediment + salinity2 + chlor2 + depth2 + DO2 + w_temp2 + turb2, data = clams)
sim_mp1 = simulateResiduals(mp1, plot = T)  ##this seems to pass?!?!
testDispersion(sim_mp1)  # dispersion = 0.95411, p-value = 0.656
testZeroInflation(sim_mp1) #ratioObsSim = Inf, p-value < 2.2e-16 ah but tons of extra zeros
#using the 'plot" function gives us some diagnostics to see whether our model fit the assumptions
plot(mp1)
## This... is not terrible, again except for the extra zeros.

## ok, so maybe try a negative binomial (we know variance>> mean, so definitely not Poisson)

p_global_nb = glmmTMB(p_afdm_mg_r ~ year + habitat+ month + sediment + salinity2 + chlor2 + depth2 + DO2 + w_temp2 + turb2 +(1|station), 
                      family = "nbinom1"(link='log'), na.action = "na.fail", data = clams)
#look at the plots (DHARMa package).  
sim_p_glob = simulateResiduals(p_global_nb, plot = T)  ##ooh, big problems with both qq and res*pred
testDispersion(sim_p_glob)  # dispersion = 6.8838, p-value = 0.008 So still pretty overdispersed.
testZeroInflation(sim_p_glob) # ratioObsSim = 0.87529, p-value = 0.416
summary(p_global_nb)


# So, maybe we can do a zero-inflated modelNB ?  

#or a zINB? Minus year and habitat. Does not converge, even when I take out the problematic variables.
p_global_zinb = glmmTMB(p_afdm_mg_r ~ month + sediment + salinity2 + chlor2 + depth2 + DO2 + w_temp2 + turb2 +(1|station), 
                        ziformula = ~ month + sediment + salinity2 + chlor2 + depth2 + DO2 + w_temp2 + turb2 , 
                        family = "nbinom1"(link='log'), na.action = "na.fail", data = clams)
sim_p_glob_zinb = simulateResiduals(p_global_zinb, plot = T)  # lots of problems
testDispersion(sim_p_glob_zinb)  # dispersion = 4.9887, p-value = 0.032, so still way overdispersed
testZeroInflation(sim_p_glob_zinb) # ratioObsSim = 0.82183, p-value = 0.24
summary(p_global_zinb)


### So. We have a zero-inflated model, but overdispersed and not fitting assumptions for a NB or ZINB model, and looks OK when log-transformed

## soooooo, can we do a a log-transformed gaussian distribution of continuous data with zero inflation?  

p_global_zi = glmmTMB(p_afdm_mg_log ~ month + sediment + salinity2 + chlor2 + depth2 + DO2 + w_temp2 + turb2, 
                      ziformula = ~ month + sediment + salinity2 + depth2 + month , 
                      family = "gaussian", na.action = "na.fail", data = clams)
sim_p_glob_zi = simulateResiduals(p_global_zi, plot = T)  # zero problems, looks awesome
testDispersion(sim_p_glob_zi)  # dispersion = 1.0354, p-value = 0.728 and the overdispersion is largely fixed I hope
testZeroInflation(sim_p_glob_zi) # ratioObsSim = 1.1613, p-value = 0.2
summary(p_global_zi)

#with random effect of station

p_global_zi = glmmTMB(p_afdm_mg_log ~ month + sediment + salinity2 + chlor2 + depth2 + DO2 + w_temp2 + turb2 +(1|station), 
                      ziformula = ~ month + sediment + salinity2 + depth2 + month , 
                      family = "gaussian", na.action = "na.fail", data = clams)
sim_p_glob_zi = simulateResiduals(p_global_zi, plot = T)  # zero problems, looks awesome
testDispersion(sim_p_glob_zi)  # dispersion = 1.0354, p-value = 0.728 and the overdispersion is largely fixed I hope
testZeroInflation(sim_p_glob_zi) # ratioObsSim = 1.1613, p-value = 0.2
summary(p_global_zi)




