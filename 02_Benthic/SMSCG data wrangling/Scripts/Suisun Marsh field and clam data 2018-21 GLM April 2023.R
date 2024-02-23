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
library(emmeans) # allows post-hoc means comparisons
library(MuMIn) #load library to do dredge model function
library(car) # to do multiple collinearity checks
library(glmmTMB) # glm fitting package - using this instead of pscl
library(DHARMa) # checking of dispersion and residuals
library(effects) # look at conditional effects, to determine detransformed means/effect sizes
library(broom) #tidies up data outputs from glms and adds columns to original data
library(ggeffects) # plotting conditional effects plots
library(mosaic) ## function "zscore" allows normalizing of continuous variables
library(GGally) ## multiple collinearity plotting ggpairs
library(data.table) ## allows fwrite function
library(Distance) ## allows QAIC comparisons

################################
## 1a: Data input: if you've already created a data file, like the EDI file, with all the fields necessary
################################
## station list
clam_in <- read.csv('Products/SMSCG_clam_EDI_2018_2021.csv')

str(clam_in)

## rename fields for easier analysis

clam_in = 
  rename(clam_in,
         year	=	 Year,
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


str(clams)

summary(clams$c_afdm_mg)

#new dataframe, counting values of FALSE and TRUE for stacked bar charts of Y_N, by month
c_clams<-clams %>% group_by(month, c_round_YN) %>% summarise(ccount=n())%>%ungroup
p_clams<-clams %>% group_by(month, p_round_YN) %>% summarise(pcount=n())%>%ungroup
t_clams<-clams %>% group_by(month, t_round_YN) %>% summarise(tcount=n())%>%ungroup
Clams_Present=c("Absent", "Present", "Absent", "Present") 
Month=c("July", "July", "September", "September")
clamcount<-cbind(Month,Clams_Present,Corbicula=c_clams$ccount,Potamocorbula=p_clams$pcount,Total=t_clams$tcount)

str(clamcount)
view(clamcount)

#second new dataframe, counting values of FALSE and TRUE for stacked bar charts of Y_N, by habitat
c_clams_hab<-clams %>% group_by(habitat, c_round_YN) %>% summarise(ccount_hab=n())%>%ungroup
p_clams_hab<-clams %>% group_by(habitat, p_round_YN) %>% summarise(pcount_hab=n())%>%ungroup
t_clams_hab<-clams %>% group_by(habitat, t_round_YN) %>% summarise(tcount_hab=n())%>%ungroup
Clams_Present_Hab=c("Absent", "Present", "Absent", "Present","Absent", "Present") 
Habitat_Sampled=c("River", "River", "Channel", "Channel", "Slough", "Slough")
clamcount_hab<-cbind(Habitat_Sampled,Clams_Present_Hab,Corbicula=c_clams_hab$ccount_hab,Potamocorbula=p_clams_hab$pcount_hab,Total=t_clams_hab$tcount_hab)
view(clamcount_hab)

################################
## 6:  EXPLORING
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

## Correlation between variables

##use basic correlation
clamscorr<-clams[,c(24:38,43)] ##create subset of only continuous variables
head(clamscorr,10)  #look at first 10 rows
res <- cor(clamscorr) ## create the correlation matrix
round(res, 2) ## look at it rounded to 2 places
## biggest Pearson correlation result here is is 0.51 between pH and DO (and 1 between salinity and salinity2)

## is there multiple co-linearity between variables? Use VIF (actually GVIF or its associate GVIF^(1/(2*df)))  to find out!
## create model for Corbicula AFDM, but is the same no matter what response variable you use.
mcafdm = lm(c_afdm ~ salinity2 + chlor2 + depth2 + DO2 + w_temp2 + turb2 + year + month + habitat +sediment , data=clams)
car::vif(mcafdm)

# > car::vif(mcafdm)
#              GVIF  Df    GVIF^(1/(2*Df))
# salinity2 3.268415  1        1.807876
# chlor2    1.520171  1        1.232952
# depth2    1.578957  1        1.256566
# DO2       2.056297  1        1.433980
# w_temp2   2.358663  1        1.535794
# turb2     1.519169  1        1.232546
# year      5.242462  3        1.318022
# month     1.549140  1        1.244645
# habitat   2.821946  2        1.296096
# sediment  1.542724  3        1.074933

#From a stackExchange discussion "The rule of GVIF(1/(2×Df))<2 is applied in some publications, which would equal to an ordinary VIF of 4 for one-coefficient variables."

## all GVIF(1/(2×Df)) are below 2, so nothing is notably multiply co-linear.

ggpairs(clams, columns = c(1,2,39,40,44:49))

#All continuous*continuous interactions are small; max is between wtemp2 and salinity2 at 0.49.
# from the ggpairs plot, it does look like year has a significant effect on salinity and wtemp (and maybe DO), and DO might also vary with sediment and habitat. 

## if we have issues with models later b/c multiple factors are saying the same thing, keep these in mind!


################################################################################

## Corbicula

# with notes on model construction from Rosie Hartman

# #A regular-old linear model probably won't work. We'll probably need a hurdle model or zero-inflated model.
# #But I'll start with showing you how regular linear models work, just 'cause.
# 
# #The "lm" function wants to you specify the model formula and give it the data set you want it to model.
# #for details on how to write formulas, see: https://cran.r-project.org/doc/manuals/r-release/R-intro.html#Formulae-for-statistical-models 
m1 = lm(log(c_afdm_mg+1)~ month + habitat, data = clams)
# #The "summary" function gives you the output
summary(m1)
# 
# #using the 'plot" function gives us some diagnostics to see whether our model fit the assumptions
plot(m1)

# 
# #most of those look OK, but the normal QQ plot is super wacky. Probably due to all the excess zeros. 
# 
# #look at partial residuals plots. THese take the effect of each factor and graph it on its own.
# visreg(m1)

## is it overdispersed?  Yup, variance way different than mean; variance much bigger than mean for AFDMs, much smaller for t_graze.
var(clams$c_afdm_mg)
mean(clams$c_afdm_mg)
var(clams$p_afdm_mg)
mean(clams$p_afdm_mg)
var(clams$t_graze_L)
mean(clams$t_graze_L)

## What if we did a dispersion test to see if it's a poisson distribution.  And no, none of them are.

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

#We are probably going to want to do a really #obnoxious negative binomial model, possibly a zero-inflated one. Sigh. (but check to see that zero-inflation is really happening!)

##################### Corbicula negative bionomials (possibly zero-inflated)

#But how do you know which predictors to include? Here is where model comparison comes in.

#set up a model with ALL your predictors, except not pH because that's never a thing (and also no orgmatter because it's missing for some points, and is captured by "sediment" categorical anyway).   use rounded version of response variable, in mg so you get even low values of biomass.
# ***Adding in random factor of station*****

#first, trying with family nbinom2

c_global_2nb = glmmTMB(c_afdm_mg_r ~ year + habitat + month + sediment + salinity2 + chlor2 + depth2 + DO2 + w_temp2 + turb2 +(1|station),
                       family = "nbinom2", na.action = "na.fail", data = clams)
#not 
# #look at the plots (DHARMa package). 
## Oooh, this is definitely problematic.  QQ and res*pred bad.
sim_c_glob_2nb = simulateResiduals(c_global_2nb, plot = T)
testDispersion(sim_c_glob_2nb)  # dispersion = 0.00021059, p-value = 0.008
testZeroInflation(sim_c_glob_2nb) #ratioObsSim = 1.2358, p-value = 0.032

#well, that did not work.  
#maybe try nbinom1?


c_global_nb = glmmTMB(c_afdm_mg_r ~ year + habitat + month + sediment + salinity2 + chlor2 + depth2 + DO2 + w_temp2 + turb2 +(1|station),
                       family = "nbinom1", na.action = "na.fail", data = clams)
# #look at the plots (DHARMa package).  An outlier, but no problems!
sim_c_glob = simulateResiduals(c_global_nb, plot = T)
testDispersion(sim_c_glob)  # dispersion = 0.54921, p-value = 0.568
testZeroInflation(sim_c_glob) #ratioObsSim = 1.0799, p-value = 0.624
summary(c_global_nb)

#let's dredge this!

# huh, year and habitat are not in any model within delta of 4 of best!
c_nb_all <- dredge(c_global_nb, rank='AICc')
view(c_nb_all)


#top single model via AIC
c_bestmod=c2_global_nb_hab = glmmTMB(c_afdm_mg_r ~ sediment + salinity2 + depth2 +(1|station),
                                     family = "nbinom1"(link='log'), na.action = "na.fail", data = clams)
sim_c_bestmod = simulateResiduals(c_bestmod, plot = T) # That does have a quantile deviation
testDispersion(sim_c_bestmod)  # dispersion = 0.44492, p-value = 0.64
testZeroInflation(sim_c_bestmod) #ratioObsSim = 1.061, p-value = 0.736
summary(c_bestmod)

#top single model
c_bestmod3=c2_global_nb_hab = glmmTMB(c_afdm_mg_r ~ sediment + salinity2 + depth2 +DO2 +(1|station),
                                     family = "nbinom1"(link='log'), na.action = "na.fail", data = clams)
sim_c_bestmod3 = simulateResiduals(c_bestmod3, plot = T) # No quantile deviation
testDispersion(sim_c_bestmod3)  # dispersion = 0.46871, p-value = 0.464
testZeroInflation(sim_c_bestmod3) #ratioObsSim = 1.0557, p-value = 0.784
summary(c_bestmod)

# model with all terms within top 2
c_bestmod2= glmmTMB(c_afdm_mg_r ~  sediment + salinity2 + depth2 + DO2 + chlor2 + w_temp2 + turb2 + month +(1|station),
                                     family = "nbinom1"(link='log'), na.action = "na.fail", data = clams)
sim_c_bestmod2 = simulateResiduals(c_bestmod2, plot = T) # Hey hey, no problems!
testDispersion(sim_c_bestmod2)  # dispersion = 0.41764, p-value = 0.504
testZeroInflation(sim_c_bestmod2) #ratioObsSim = 1.0561, p-value = 0.736
summary(c_bestmod2)

## try for model averaging?
summary(model.avg(c_nb_all, subset = delta <= 2)) #AvgAICc for 6 averaged models of 2783.14

AICc(c_bestmod, c_bestmod2, c_bestmod3, c_global_nb)
# df     AICc
# c_bestmod    8 2782.008
# c_bestmod2  13 2791.418
# c_bestmod3   9 2783.425
# c_global_nb 18 2798.543

AIC(c_bestmod, c_bestmod2, c_bestmod3, c_global_nb)
# df      AIC
# c_bestmod    8 2781.339
# c_bestmod2  13 2789.685
# c_bestmod3   9 2782.584
# c_global_nb 18 2795.207


## OK, we go for c_bestmod or c_bestmod3? they're functionally the same.
summary(c_bestmod)
summary(c_bestmod3)
# and emmeans or ggemeans?  estimates are functionally the same too.  Double check in other models with >1 categorical variable.
emmeans(c_bestmod, pairwise~ sediment, component = "cond", type = "response")
ggemmeans(c_bestmod, terms = "sediment", type = "fe")

###############################

##POTAMOCORBULA

## look at the data
ggplot(clams) + geom_histogram(aes(x = p_afdm_mg))
# That's a whole lot of zeros!
ggplot(clams) + geom_histogram(aes(x = log(p_afdm_mg+1)))
## Still kind of gross, but a somewhat better distribution has appeared to the right, in addition to the zeros!

## check distributions
mp1 = lm(log(p_afdm_mg+1)~habitat+ month + sediment + salinity2 + chlor2 + depth2 + DO2 + w_temp2 + turb2, data = clams)
#The "summary" function gives you the output
sim_mp1 = simulateResiduals(mp1, plot = T)  ##this seems to pass?!?!
testDispersion(sim_mp1)  # dispersion = 0.95411, p-value = 0.656
testZeroInflation(sim_mp1) #ratioObsSim = Inf, p-value < 2.2e-16 ah but tons of extra zeros
#using the 'plot" function gives us some diagnostics to see whether our model fit the assumptions
plot(mp1)
## This... is not terrible, except for the extra zeros.


## ok, so maybe try a negative binomial (we know variance>> mean, so definitely not Poisson)

p_global_nb = glmmTMB(p_afdm_mg_r ~ year + habitat+ month + sediment + salinity2 + chlor2 + depth2 + DO2 + w_temp2 + turb2 +(1|station), 
                      family = "nbinom1"(link='log'), na.action = "na.fail", data = clams)
#look at the plots (DHARMa package).  
sim_p_glob = simulateResiduals(p_global_nb, plot = T)  ##ooh, problems with both qq and res*pred
testDispersion(sim_p_glob)  # dispersion = 7.7253, p-value < 2.2e-16 So still pretty overdispersed. 
testZeroInflation(sim_p_glob) # ratioObsSim = 0.86315, p-value = 0.328 but weirdly not id'd as zero inflated?
summary(p_global_nb)


# What about if we use nbinom2, which is quadratic parameterization?  Would that help with the persistent overdispersion and lack of q:q res:pred fit?

p_global_nb2 = glmmTMB(p_afdm_mg_r ~ year + habitat+ month + sediment + salinity2 + chlor2 + depth2 + DO2 + w_temp2 + turb2 +(1|station), 
                      family = "nbinom2"(link='log'), na.action = "na.fail", data = clams)
#look at the plots (DHARMa package).  
sim_p_glob2 = simulateResiduals(p_global_nb2, plot = T)  ##ooh, problems with res*pred
testDispersion(sim_p_glob2)  # dispersion = 0.015203, p-value = 0.584 okay! 
testZeroInflation(sim_p_glob2) # ratioObsSim = ratioObsSim = 1.4428, p-value = 0.008 

#So, nbinom2, but try adding zero-inflation.

p_globzi_nb2 = glmmTMB(p_afdm_mg_r ~ year + habitat+ month + sediment + salinity2 + chlor2 + depth2 + DO2 + w_temp2 + turb2 +(1|station), 
                       ziformula = ~ depth2 + salinity2,
                       family = "nbinom2"(link='log'), na.action = "na.fail", data = clams)
#look at the plots (DHARMa package).  
sim_p_globzi2 = simulateResiduals(p_globzi_nb2, plot = T)  ##ooh, problems with res*pred
testDispersion(sim_p_globzi2)  # dispersion = 0.0063103, p-value = 0.888 okay! 
testZeroInflation(sim_p_globzi2) # ratioObsSim = 1.1324, p-value = 0.328 and adding salinity helps with ZI

#what if we use a truncated ZI nonbinomial2?

p_globzi_2tnb = glmmTMB(p_afdm_mg_r ~ year + habitat + month + sediment + salinity2 + chlor2 + depth2 + DO2 + w_temp2 + turb2 +(1|station), 
                       ziformula = ~  depth2 + salinity2,   
                       data = clams, family = truncated_nbinom2, na.action = "na.fail")
sim_p_globzi_2tnb = simulateResiduals(p_globzi_2tnb, plot = T) #no issues!
testDispersion(sim_p_globzi_2tnb)  # dispersion = 0.32337, p-value = 0.912
testZeroInflation(sim_p_globzi_2tnb) #ratioObsSim = 0.99732, p-value = 1  of course, there's no zeros

#ok, let's try dredging that

#need to re-dredge - used the wrong depth metric for ZI formula first time.  But it's probably not too off.

p_tnb2_all <- dredge(p_globzi_2tnb, rank='AICc')
view(p_tnb2_all)



fwrite(p_tnb2_all,"Products/Suisun Marsh Potamocorbula model selection, ZI NB2.csv", row.names=FALSE)

#this is just the top-rated model.
p_2tnb1 = glmmTMB(p_afdm_mg_r ~ year + month + habitat  + salinity2 + depth2 + (1|station), 
                        ziformula = ~  depth2 + salinity2,   
                        data = clams, family = truncated_nbinom2, na.action = "na.fail")
sim_p_2tnb1 = simulateResiduals(p_2tnb1, plot = T) #okay!
testDispersion(sim_p_2tnb1)  # dispersion = 0.27806, p-value = 0.768

# ok, but we didn't put a lot of things in the ZI term because the model choice took forever.  Let's get an idea of what might else might need to go in.
p_zeros= glm(p_round_YN~ year +month + habitat +salinity2 +depth2 +DO2 +chlor2+DO2 +w_temp2,
             data=clams, family =binomial)
summary(p_zeros)
#from this, year, salinity, depth, and maybe chlor and habitat had decent predictive power of zeros.  Try them in the 

# good, let's try a few more possible terms in the zero term
p_2tnb2 = glmmTMB(p_afdm_mg_r ~ year + month + habitat  + salinity2 + depth2 + (1|station), 
                  ziformula = ~  depth2 + salinity2 +year,   
                  data = clams, family = truncated_nbinom2, na.action = "na.fail")
sim_p_2tnb2 = simulateResiduals(p_2tnb2, plot = T) #okay!
testDispersion(sim_p_2tnb2)  # dispersion = 0.3078, p-value = 0.792

p_2tnb3 = glmmTMB(p_afdm_mg_r ~ year + month + habitat  + salinity2 + depth2 + (1|station), 
                  ziformula = ~  depth + salinity2 +year +habitat,   
                  data = clams, family = truncated_nbinom2, na.action = "na.fail")
sim_p_2tnb3 = simulateResiduals(p_2tnb3, plot = T) #okay!
testDispersion(sim_p_2tnb3)  # dispersion = 0.34822, p-value = 0.864

p_2tnb4 = glmmTMB(p_afdm_mg_r ~ year + month + habitat  + salinity2 + depth2 + (1|station), 
                  ziformula = ~  depth2 + salinity2 +year +habitat +chlor2,   
                  data = clams, family = truncated_nbinom2, na.action = "na.fail")
sim_p_2tnb4 = simulateResiduals(p_2tnb4, plot = T) #okay!
testDispersion(sim_p_2tnb4)  # dispersion = 0.25422, p-value = 0.776


AIC(p_2tnb1, p_2tnb2, p_2tnb3, p_2tnb4)

# df      AIC
# p_2tnb1 14 2722.894
# p_2tnb2 17 2715.943
# p_2tnb3 19 2708.256
# p_2tnb4 20 2704.255

AICc(p_2tnb1, p_2tnb2, p_2tnb3, p_2tnb4)

# df     AICc
# p_2tnb1 14 2724.904
# p_2tnb2 17 2718.914
# p_2tnb3 19 2711.981
# p_2tnb4 20 2708.393

BIC(p_2tnb1, p_2tnb2, p_2tnb3, p_2tnb4)
# df      BIC
# p_2tnb1 14 2770.658
# p_2tnb2 17 2773.941
# p_2tnb3 19 2773.077
# p_2tnb4 20 2772.488

## great, I'll try a few more terms in the conditional
p_2tnb5 = glmmTMB(p_afdm_mg_r ~ year + month + habitat  + salinity2 + depth2 +  chlor2 +(1|station), 
                  ziformula = ~  depth2 + salinity2 +year +habitat +chlor2,   
                  data = clams, family = truncated_nbinom2, na.action = "na.fail")
sim_p_2tnb5 = simulateResiduals(p_2tnb5, plot = T) #okay!
testDispersion(sim_p_2tnb5)  # dispersion = 0.17388, p-value = 0.824

p_2tnb6 = glmmTMB(p_afdm_mg_r ~ year + month + habitat  + salinity2 + depth2 +  DO2 +(1|station), 
                  ziformula = ~  depth2 + salinity2 +year +habitat +chlor2,   
                  data = clams, family = truncated_nbinom2, na.action = "na.fail")
sim_p_2tnb6 = simulateResiduals(p_2tnb6, plot = T) #okay!
testDispersion(sim_p_2tnb6)  # dispersion = 0.40792, p-value = 0.88

p_2tnb7 = glmmTMB(p_afdm_mg_r ~ year + month + habitat  + salinity2 + depth2 +  DO2 +chlor2 + (1|station), 
                  ziformula = ~  depth2 + salinity2 +year +habitat +chlor2,   
                  data = clams, family = truncated_nbinom2, na.action = "na.fail")
sim_p_2tnb7 = simulateResiduals(p_2tnb7, plot = T) #okay!
testDispersion(sim_p_2tnb7)  # dispersion = 0.36155, p-value = 0.904

AIC(p_globzi_2tnb, p_2tnb1, p_2tnb2, p_2tnb3, p_2tnb4, p_2tnb5, p_2tnb6, p_2tnb7)
# df      AIC
# p_globzi_2tnb 21 2734.936
# p_2tnb1       14 2722.894
# p_2tnb2       17 2715.943
# p_2tnb3       19 2708.256
# p_2tnb4       20 2704.255
# p_2tnb5       21 2705.965
# p_2tnb6       21 2705.222
# p_2tnb7       22 2707.217

AICc(p_globzi_2tnb, p_2tnb1, p_2tnb2, p_2tnb3, p_2tnb4, p_2tnb5, p_2tnb6, p_2tnb7)
# df     AICc
# p_globzi_2tnb 21 2739.510
# p_2tnb1       14 2724.904
# p_2tnb2       17 2718.914
# p_2tnb3       19 2711.981
# p_2tnb4       20 2708.393
# p_2tnb5       21 2710.539
# p_2tnb6       21 2709.797
# p_2tnb7       22 2712.251

BIC(p_globzi_2tnb, p_2tnb1, p_2tnb2, p_2tnb3, p_2tnb4, p_2tnb5, p_2tnb6, p_2tnb7)
# df      BIC
# p_globzi_2tnb 21 2806.580
# p_2tnb1       14 2770.658
# p_2tnb2       17 2773.941
# p_2tnb3       19 2773.077
# p_2tnb4       20 2772.488
# p_2tnb5       21 2777.610
# p_2tnb6       21 2776.867
# p_2tnb7       22 2782.273

#Ok, looks likd p_2tnb4 is our best.
summary(p_2tnb4)

#ways of means testing; start with basic emmeans, averaged/adjusted across any other categorical factors.  Which ggpredict does not do.
emmeans(p_2tnb4, pairwise~ year, component = "cond", type = "response") 
# ggemeans() refines on emmeans - also allows effects also of continuous factors in model.  Since continuous here is scaled, this has 0 effect here, but would be very cool.
ggemmeans(p_2tnb4, terms = "year", type = "fe") #same as emmeans(p_2tnb4, pairwise~ habitat, component = "cond", type = "response")
# What if
# ggemmeans(p_2tnb4, terms = "year", type = "fe", weights = "proportional") #this would make averaging over "habitat" proportional to the numbers within each level of habitat.  Which we don't want.
# ggemmeans(p_2tnb4, terms = "year", type = "fe.zi") #a bit different from emmeans, all lower CI at zero.  Conditions on both fixed effects and ZI components.  I don't think we want this?  We're just looking at fixed effects.
# ggemmeans(p_2tnb4, terms = "year", type = "random") #much bigger CI b/c of random effects


emmeans(p_2tnb4, pairwise~ habitat, component = "cond", type = "response")
emmeans(p_2tnb4, pairwise~ year, component = "zi", type = "response") 
emmeans(p_2tnb4, pairwise~ habitat, component = "zi", type = "response") 


##############################
#3 TOTAL GRAZING


## look at the data
ggplot(clams) + geom_histogram(aes(x = t_graze_L))

# That's a whole lot of zeros!
ggplot(clams) + geom_histogram(aes(x = log(t_graze_L +1)))
## Still really kind of gross.
#Huh.  Now this looks a little more like something.

## check distributions
mt1 = lm(log(t_graze_L+1)~ year + month + habitat +depth, data = clams)
#The "summary" function gives you the output
summary(mt1)

#using the 'plot" function gives us some diagnostics to see whether our model fit the assumptions
plot(mt1)
## lower QQ kind of weird, scale/location weird, but... not too bad?
sim_mt1 = simulateResiduals(mt1, plot = T) #res*pred is bad
testDispersion(sim_mt1)  # dispersion = 0.96503, p-value = 0.744
testZeroInflation(sim_mt1) # ratioObsSim = Inf, p-value < 2.2e-16 so some significant ZI
mt1_eff = allEffects(mt1)
plot(mt1_eff)

#just log-transformed

mt2 = lm(log(t_graze_L+1)~  year + month + habitat +depth, data = clams)
sim_mt2 = simulateResiduals(mt2, plot = T) #fail quantile tests
testDispersion(sim_mt2)  # dispersion = 0.96503, p-value = 0.744
testZeroInflation(sim_mt2) #ratioObsSim = Inf, p-value < 2.2e-16 LOTS of ZI
mt2_eff = allEffects(mt2)
plot(mt2_eff)

#OK, so maybe a NB? take out wtemp2 because it's essentially the same info as month.

t_global_nb = glmmTMB(t_graze_L_r ~ year + habitat+ month + sediment + salinity2 + chlor2 + depth2 + DO2 + turb2 +(1|station), 
                      family = "nbinom1", na.action = "na.fail", data = clams)

sim_t_glob_nb = simulateResiduals(t_global_nb, plot = T) #QQ deviating - 0.048 but res:pred is not too bad.
testDispersion(sim_t_glob_nb)  # dispersion = 2.0344, p-value = 0.048
testZeroInflation(sim_t_glob_nb) #ratioObsSim = 0.9029, p-value = 0.696
summary(t_global_nb)

# Dredge it!
t_allmodels = dredge(t_global_nb)
library(effects)
library(broom)
t_glob_eff = allEffects(t_global_nb)
plot(t_glob_eff)
summary(t_global_nb)
view(t_allmodels)

## ok, try dredge + raink by QAIC, since it's quasi

t_chat<-3031.9/206 ## global model variance/global model residual df 
t_nb_all_q <- dredge(t_global_nb, rank='QAIC', chat=t_chat)
view(t_nb_all_q)


#best model fails 
t_nb_best = glmmTMB(t_graze_L_r ~ sediment +depth2 + DO2 +(1|station), 
                     family = "nbinom1", na.action = "na.fail", data = clams)
sim_t_nb_best = simulateResiduals(t_nb_best, plot = T) # res:pred and q:q fail 
testDispersion(sim_t_nb_best)  # dispersion = 1.9709, p-value = 0.04
testZeroInflation(sim_t_nb_best) #ratioObsSim = 0.97393, p-value = 1


#model with everything under AICc delta 3
t_nb_1 = glmmTMB(t_graze_L_r ~ sediment +depth2 + DO2 +habitat + chlor2 +month + salinity2+ turb2+ (1|station), 
                    family = "nbinom1", na.action = "na.fail", data = clams)
sim_t_nb_1 = simulateResiduals(t_nb_1, plot = T) # passes!
testDispersion(sim_t_nb_1)  # dispersion = 1.8932, p-value = 0.08 passes but barely.  Still kind of overdispersed.
testZeroInflation(sim_t_nb_1) #ratioObsSim = 0.8719, p-value = 0.664

#model with only the few strongest things 
t_nb_2 = glmmTMB(t_graze_L_r ~ sediment +depth2 + DO2 + habitat +month + salinity2+ turb2+ (1|station), 
                 family = "nbinom1", na.action = "na.fail", data = clams)
sim_t_nb_2 = simulateResiduals(t_nb_2, plot = T) # passes!
testDispersion(sim_t_nb_2)  # dispersion = dispersion = 2.0015, p-value = 0.08.  Still kind of overdispersed.
testZeroInflation(sim_t_nb_2) #ratioObsSim = 0.89335, p-value = 0.712

#model with only the even a fewer few strongest things 
t_nb_3 = glmmTMB(t_graze_L_r ~ sediment +depth2 + DO2  + salinity2+ turb2+ (1|station), 
                 family = "nbinom1", na.action = "na.fail", data = clams)
sim_t_nb_3 = simulateResiduals(t_nb_3, plot = T) # passes!
testDispersion(sim_t_nb_3)  # dispersion = 1.8349, p-value = 0.12
testZeroInflation(sim_t_nb_3) #ratioObsSim = 0.89779, p-value = 0.784

#any hope w negbinom2? nope!
# t_nb2_1 = glmmTMB(t_graze_L_r ~ sediment +depth2 + DO2 +habitat + chlor2 +month + salinity2+ turb2+ (1|station), 
#                  family = "nbinom2", na.action = "na.fail", data = clams)
# sim_t_nb2_1 = simulateResiduals(t_nb2_1, plot = T) # fails appallingly
# testDispersion(sim_t_nb2_1)  # dispersion = 0.061294, p-value = 0.048
# testZeroInflation(sim_t_nb2_1) #ratioObsSim = 1.7065, p-value = 0.008




## model comparison!
AIC(t_nb_1, t_nb_2, t_nb_3)
# df      AIC
# t_nb_1 14 3064.147
# t_nb_2 13 3062.745
# t_nb_3 10 3058.566

AICc(t_nb_1, t_nb_2, t_nb_3)
# df     AICc
# t_nb_1 14 3066.156
# t_nb_2 13 3064.478
# t_nb_3 10 3059.599
BIC(t_nb_1, t_nb_2, t_nb_3)
# df      BIC
# t_nb_1 14 3111.910
# t_nb_2 13 3107.096
# t_nb_3 10 3092.683
QAIC(t_nb_1, t_nb_2, t_nb_3)

# ##Looks like t_nb_3 fits best with AIC, , AICc, and   BIC, and is also OK on testing!  So we'll go with that.

summary(t_nb_3)
emmeans(t_nb_3, pairwise~sediment, component = "cond", type = "response")



################################################################################
## CHARTS

# Overall theme for clam charts - Horizontal +vertical.  
clamtheme <- theme(panel.background = element_rect(fill = "white", color ="black", size=1),
                panel.grid.major.x = element_line(colour="grey", size = (0.2)),
                panel.grid.major.y = element_line(colour="grey", size = (0.2)),
                plot.title = element_text(size = (20)),
                axis.title = element_text(size = (15)),
                axis.text = element_text(size = (15)),
                legend.title=element_text(size=15), 
                legend.text=element_text(size=15))

# Overall theme for clam charts - Horizonal lines only
clamtheme2 <- theme(panel.background = element_rect(fill = "white", color ="black", size=1),
                   panel.grid.major.x = element_line(colour="grey", size = (0.2)),
                   panel.grid.major.y = element_blank(),
                   plot.title = element_text(size = (20)),
                   axis.title = element_text(size = (15)),
                   axis.text = element_text(size = (15)),
                   legend.title=element_text(size=15), 
                   legend.text=element_text(size=15))

# Overall theme for clam charts - Vertical lines only
clamtheme3 <- theme(panel.background = element_rect(fill = "gray95", color ="black", size=1),
                    panel.grid.major.x = element_blank(),
                    panel.grid.major.y = element_line(colour="grey", size = (0.2)),
                    plot.title = element_text(size = (20)),
                    axis.title = element_text(size = (15)),
                    axis.text = element_text(size = (15)),
                    legend.title=element_text(size=15), 
                    legend.text=element_text(size=15))

#################                   
## depth by habitat
plot_depth<-ggplot(data=clams, aes(x = habitat, y = depth, fill=habitat)) + geom_boxplot() 

plot_depth + clamtheme2 + ggtitle("Water depth by habitat")+
  labs(y="Water depth (ft)", x = "Habitat") +
  scale_fill_manual(values = c("blue", "green", "brown"))

## depth histogram
hist_depth<-ggplot(data =clams, aes(x = depth)) + geom_histogram(fill = "dodgerblue") 

hist_depth +clamtheme +ggtitle("Water Depth")+
  labs(x="Water depth (ft)")

################
## chlorophyll histogram
hist_depth<-ggplot(data =clams, aes(x = chlor)) + geom_histogram(fill = "darkolivegreen") 

hist_depth +clamtheme +ggtitle("Chlorophyll")+
  labs(x="chlorophyll (ug/mL)")

#chlorophyll by habitat
plot_chlor<-ggplot(data=clams, aes(x = habitat, y = chlor, fill=habitat)) + geom_boxplot() 

plot_chlor + clamtheme + ggtitle("Chlorophyll a by habitat")+
  labs(y="Chlorophyll (ug/mL))", x = "Habitat") +
  scale_fill_manual(values = c("blue", "green", "brown"))

#temp by month
plot_temp<-ggplot(data=clams, aes(x = month, y = w_temp, fill=month)) + geom_boxplot() 

plot_temp + clamtheme +  labs(y="Water temp (by month(C)", x = "Month") +
  scale_fill_manual(values = c("blue", "green"))

#############
## turbidity by habitat
plot_turb<-ggplot(data=clams, aes(x = habitat, y = turb, fill=habitat)) + geom_boxplot() 

plot_turb + clamtheme + ggtitle("Turbidity by habitat")+
  labs(y="Turbidity (NTU))", x = "Habitat") +
  scale_fill_manual(values = c("blue", "green", "brown"))

######################
#temp by month
plot_temp<-ggplot(data=clams, aes(x = month, y = w_temp, fill=month)) + geom_boxplot() 

plot_temp + clamtheme +  labs(y="Water temp (by month(C)", x = "Month") +
  scale_fill_manual(values = c("blue", "green"))

##################

##### Corbicula conditional model effects

## use the best model and ggeffect() and choose type = simulate, because that apparently also includes the random variability in the model
#using ggeffect() instead of ggpredict() because that applies across averaged non-focal categorical variables in the model; ggpredict appears to be only using the model's reference levels of non-focal categorical variables, and for Potamocorbula was coming up with predictions 100-500x too high compared with original data.

#Let's do it one term at a time

#first predict the values and confidence intervals of the model
#it automatically back-transforms the clams for you

c_salmod1 = ggeffect(c_bestmod, terms = "salinity2", type = "fe") 
summary(c_salmod)
#how about with random effects included? 
c_salmod_r = ggeffect(c_bestmod, terms = "salinity2", type = "random") 
summary(c_salmod_r)
#type = simulate includes all kinds of uncertainty, including ZI and random? same as fe.  so use simulate
c_salmod = ggeffect(c_bestmod, terms = "salinity2", type = "simulate") 
summary(c_salmod)

# 
# #with ggpredict as comparison.  Not very different here, probably because there's just one categorical variable.
# c_salmod_p = ggpredict(c_bestmod, terms = "salinity2", type = "fe") 
# summary(c_salmod_p)

#now create a formula to back-transform salinity
#Let's do it one term at a time

sal = lm(salinity ~ salinity2, data = clams) %>%
  tidy()
str(sal)

#backtransform salinity
c_salmod= mutate(c_salmod, salinity = x*sal$estimate[2]+ sal$estimate[1])

#plot it, with conversions of /1000 to get from mg back to g
ggplot(c_salmod, aes(x = salinity, y = (predicted/1000)))+
  geom_line()+
  geom_ribbon(aes(ymin = (conf.low/1000), ymax = (conf.high/1000)), alpha = 0.5, fill = "orchid2")+
  theme_bw() + ylab("Corbicula ash-free dry mass (g AFDM/m2)")+xlab("Conductivity (uS/cm)")


#sediment
c_smod = ggeffect(c_bestmod, terms = "sediment", type = "simulate")

#plot it
ggplot(c_smod, aes(x = x, y = (predicted/1000)))+
  geom_point()+
  geom_errorbar(aes(ymin = (conf.low/1000), ymax = (conf.high/1000)), width = 0.5)+
  theme_bw() + ylab("Corbicula biomass (g AFDM/m2)")+xlab("Sediment type")


#depth
c_dmod = ggeffect(c_bestmod, terms = "depth2", type = "simulate")   

#now create a formula to back-transform depth
dp= lm(depth ~ depth2, data = clams) %>%
  tidy()

#add backtransformed depth to model effects
c_dmod= mutate(c_dmod, depth = x*dp$estimate[2]+ dp$estimate[1])

#plot it
ggplot(c_dmod, aes(x = depth, y = (predicted/1000)))+
  geom_line()+
  geom_ribbon(aes(ymin = (conf.low/1000), ymax = (conf.high/1000)), alpha = 0.5, fill = "dodgerblue2")+
  theme_bw() + ylab("Corbicula biomass (g AFDM/m2)")+xlab("Water depth (m)")

c_dmod <- ggplot(clams, aes(x = depth, y = c_afdm)) + 
                 geom_point(size=3, color="dodgerblue2")
c_dmod + clamtheme + labs(x="Corbicula fluminea", y = "Water depth (m)") +
  guides(fill = "none", color = "none")

##################



######### Potamocorbula effects plots

# general rules I've found:
## ggeffect for: continuous variables in the conditional part of a model
## ggemmeans for: categorical variables in the conditional part of model (accounts for continuous variables, which emmeans doesn't do alas)
## ggpredict for: continous or categorical variables (with type = zi.prob) in the zero part of a model


## Potam salinity conditional model

#first calculate the values and confidence intervals of the model
#it automatically back-transforms the clams for you
# not using fe.zi because I don't want to condition the responses here on the ZI component; I look at that separately with zi.prob
#using ggeffect() instead of ggpredict() because that applies across averaged non-focal categorical variables in the model; ggpredict appears to be only using the model's reference levels of non-focal categorical variables, and was coming up with predictions 100-500x too high compared with original data.

p_salmod = ggeffect(p_2tnb4, terms = "salinity2", type = "fe") 
summary(p_salmod)
summary(clams$p_afdm_mg_r)


#backtransform salinity
sal = lm(salinity ~ salinity2, data = clams) %>%
  tidy()
str(sal)

#add back-transformed salinity to model confidence intervals
p_salmod= mutate(p_salmod, salinity = x*sal$estimate[2]+ sal$estimate[1])

#plot it, with conversions of /1000 to get from mg back to g
# note that coord_cartesian just changes the visible area, not the points included - that would be xlim or scale_x_continuous, which remove points from ribbon/prediction outside of limits 
ggplot(p_salmod, aes(x = salinity, y = (predicted/1000)))+  geom_line()+
  geom_ribbon(aes(ymin = (conf.low/1000), ymax = (conf.high/1000)), alpha = 0.5, fill = "orchid2")+
  theme_bw() + ylab("Potamocorbula ash-free dry mass (g/m2)")+xlab("Conductivity (uS/cm)") 


#But here for zero-probability plots, use ggpredict.  Otherwise it seems to be trying to calculate marginal effects.  
## Potam salinity zeros

p_salmod_zi = ggpredict(p_2tnb4, terms = "salinity2", type = "zi.prob") 

#backtransform salinity
p_salmod_zi= mutate(p_salmod_zi, salinity = x*sal$estimate[2]+ sal$estimate[1])

#plot it
ggplot(p_salmod_zi, aes(x = salinity, y = predicted))+
  geom_line()+
  geom_ribbon(aes(ymin = (conf.low), ymax = (conf.high)), alpha = 0.5, fill = "orchid2")+
  theme_bw() + ylab("Probability of zero Potamocorbula")+xlab("Conductivity (uS/cm)") +
  coord_cartesian(xlim = c(0, 22500)) 


## Potam depth, conditional effects
p_dmod = ggeffect(p_2tnb4, terms = "depth2", type = "fe") 
summary(p_dmod)

dp= lm(depth ~ depth2, data = clams) %>%
  tidy()

p_dmod= mutate(p_dmod, depth = x*dp$estimate[2]+ dp$estimate[1])

#plot it, dividing all y values by 1000 to get from mg to g
ggplot(p_dmod, aes(x = depth, y = (predicted/1000)))+
  geom_line()+
  geom_ribbon(aes(ymin = (conf.low/1000), ymax = (conf.high/1000)), alpha = 0.5, fill = "dodgerblue2")+
  theme_bw() + ylab("Potamocorbula ash-free dry mass (g/m2)")+ xlab("Depth (m)")


#Potam depth, zeros

p_dmod_zi = ggpredict(p_2tnb4, terms = "depth2", type = "zi.prob") 

dp= lm(depth ~ depth2, data = clams) %>%
  tidy()

p_dmod_zi= mutate(p_dmod_zi, depth = x*dp$estimate[2]+ dp$estimate[1])

ggplot(p_dmod_zi, aes(x = depth, y = predicted))+
  geom_line()+
  geom_ribbon(aes(ymin = (conf.low), ymax = (conf.high)), alpha = 0.5, fill = "dodgerblue2")+
  theme_bw() + ylab("Probability of zero Potamocorbula")+ xlab("Depth (m)")

#Potam chlorophyll, zeros

p_cmod_zi = ggpredict(p_2tnb4, terms = "chlor2", type = "zi.prob") 

dp= lm(depth ~ depth2, data = clams) %>%
  tidy()

p_cmod_zi= mutate(p_cmod_zi, depth = x*dp$estimate[2]+ dp$estimate[1])

ggplot(p_cmod_zi, aes(x = depth, y = predicted))+
  geom_line()+
  geom_ribbon(aes(ymin = (conf.low), ymax = (conf.high)), alpha = 0.5, fill = "green")+
  theme_bw() + ylab("Probability of zero Potamocorbula")+ xlab("Chlorophyll, (µg/L)")


# Potamocorbula habitat conditional
# using ggemmeans to calculate, since those (unlike ggpredict) don't use only reference level of other non-focal categorical variables

p_hmod = ggemmeans(p_2tnb4, terms = "habitat", type = "fe") #same means and CIs as emmeans(p_2tnb4, pairwise~ habitat, component = "cond", type = "response")
summary(p_hmod)
emmeans(p_2tnb4, pairwise~ habitat, component = "cond", type = "response")


ggplot(p_hmod, aes(x = x, y = (predicted/1000)))+
  geom_point()+
  geom_errorbar(aes(ymin = (conf.low/1000), ymax = (conf.high/1000)), width=0.5)+
  theme_bw() + ylab("Potamocorbula ash-free dry mass (g/m2)")+xlab("Habitat")


# Potamocorbula habitat zeros

p_hmod_zi = ggpredict(p_2tnb4, terms = "habitat", type = "zi.prob")

#plot it
ggplot(p_hmod_zi, aes(x = x, y = (predicted)))+
  geom_point()+
  geom_errorbar(aes(ymin = (conf.low), ymax = (conf.high)), width = 0.5)+
  theme_bw() + ylab("Probability of zero Potamocorbula")+xlab("Habitat")


# Potamocorbula year conditional

p_ymod = ggemmeans(p_2tnb4, terms = "year", type = "fe")

#plot it
ggplot(p_ymod, aes(x = x, y = (predicted/1000)))+
  geom_point()+
  geom_errorbar(aes(ymin = (conf.low/1000), ymax = (conf.high/1000)), width = 0.5)+
  theme_bw() + ylab("Potamocorbula ash-free dry mass (g/m2)")+xlab("Year")

# Potamocorbula year zeros

p_ymod_zi = ggpredict(p_2tnb4, terms = "year", type = "zi.prob")

#plot it
ggplot(p_ymod_zi, aes(x = x, y = (predicted)))+
  geom_point()+
  geom_errorbar(aes(ymin = (conf.low), ymax = (conf.high)), width = 0.5)+
  theme_bw() + ylab("Probability of zero Potamocorbula")+xlab("Year")


#Potam month conditional
p_mmod = ggeffect(p_2tnb4, terms = "month", type = "fe")

#plot it
ggplot(p_mmod, aes(x = x, y = (predicted/1000)))+
  geom_point()+
  geom_errorbar(aes(ymin = (conf.low/1000), ymax = (conf.high/1000)), width = 0.5)+
  theme_bw() + ylab("Corbicula biomass (g AFDM/m2)")+xlab("Month")


# #############################################################


# ## Total grazing 

## Total grazing depth, conditional effects
t_dmod = ggeffect(t_nb_3, terms = "depth2", type = "fe") 
summary(t_dmod)

dp= lm(depth ~ depth2, data = clams) %>%
  tidy()

t_dmod= mutate(t_dmod, depth = x*dp$estimate[2]+ dp$estimate[1])

#plot it, dividing all y values by 1000 to get from L to m3
ggplot(t_dmod, aes(x = depth, y = (predicted/1000)))+
  geom_line()+
  geom_ribbon(aes(ymin = (conf.low/1000), ymax = (conf.high/1000)), alpha = 0.5, fill = "dodgerblue2")+
  theme_bw() + ylab("Total grazing (m3/m2/day)")+ xlab("Depth (m)")

# Total grazing by sediment type

t_smod = ggemmeans(t_nb_3, terms = "sediment", type = "fe") #same means and CIs as emmeans
summary(t_smod)
emmeans(t_nb_3, pairwise~ sediment, component = "cond", type = "response")


ggplot(t_smod, aes(x = x, y = (predicted/1000)))+
  geom_point()+
  geom_errorbar(aes(ymin = (conf.low/1000), ymax = (conf.high/1000)), width=0.5)+
  theme_bw() + ylab("Total grazing (m3/m2/day)")+xlab("Sediment")

