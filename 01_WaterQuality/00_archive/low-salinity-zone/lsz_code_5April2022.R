# EZ Mapping (clean version)
# Code and csv files stored on Github here: https://github.com/emp-des/LowSalinityZone
# by Dennis Finger
#devtools::install_github("InteragencyEcologicalProgram/deltamapr")#if needed
packs<-c("lubridate","tidyverse","sf","ggpubr","broom","AICcmodavg","spacetools",
         "igraph","devtools","zoo","gridExtra","deltamapr","rgeos",
         "rnaturalearth",'rnaturalearthdata',"car","emmeans","dunn.test",
         "mapdata","ggmap","maps")
lapply(packs, require, character.only = TRUE)
#change working directory to where your csv's are stored locally
setwd("C:/Users/dfinger/OneDrive - California Department of Water Resources/My Documents/EZ_mapping")
edi_2014<-read.csv("edi_dataset_wRiverKms_post2014.csv")#can be found on github

#Sections 1 and 2 create datasets, but they are not necessary to run if importing csv's from Github

##### Section 1: creating datasets for river kiloms combined with bryte/field analytes #####
river_kms<-read.csv("ez_stations_distances.csv")
river_kms$SampleDate<-mdy(river_kms$SampleDate)
river_kms$EZStation<-substring(river_kms$EZStation,3)
river_kms$river_km<-as.numeric(as.character(river_kms$river_km))

#plot(data=river_kms,river_km~mdy(SampleDate),col=ifelse(mdy(SampleDate)==mdy("10/14/2021")&EZStation=="EZ2SJR","red","black"))
ggplot(data=river_kms,aes(x=mdy(SampleDate),y=river_km))+
  geom_point(aes(color=ifelse(mdy(SampleDate)==mdy("10/14/2021")&EZStation=="EZ2SJR","red","black")))+
  labs(color=element_blank(),x="Date",y="River kilometers")+
  theme(legend.position = "top")+
  scale_color_manual(values =c('black'='black','red'='red'), labels = c('EZ Points','10/21 EZ2SJR'))+
  theme(axis.title=element_text(size=12),
        axis.text=element_text(size=11),
        legend.text = element_text(size=11))

#using 76km as location cutoff, same as in Arthur's code
river_kms<-river_kms%>%
  add_column(location=ifelse(nchar(river_kms$EZStation)>3,"sjr",
                             ifelse(river_kms$river_km>76,"sac","joined")))
river_kms<-river_kms%>%
  add_column(loc_num=ifelse(nchar(river_kms$EZStation)>3,1,
                            ifelse(river_kms$river_km>76,2,3)))
edi_bryte<-read.csv("EDI_w_2021bryte.csv")
edi_bryte$Date<-mdy(edi_bryte$Date)
edi_bryte$Station<-gsub("-","",edi_bryte$Station)
edi_sub<-filter(edi_bryte,edi_bryte$Date>mdy(12312003)&edi_bryte$Date<mdy(11012021)&north_lat!="NA")
edi_sub['river_km']<-NA
edi_sub['location']<-NA
edi_sub['loc_num']<-NA
#can further subset down to post-2014 (after sjr) or only sac; could also eventually include pre-2014 analyte vals too

#eventually might save thsi csv and load, rather than running every time
#combining river_kms and edi_bryte
for (y in 1:nrow(edi_sub)) {
  for (z in 1:nrow(river_kms)){
    if(edi_sub[y,2]==river_kms[z,2]&edi_sub[y,1]==river_kms[z,1]){
      edi_sub[y,49]<-river_kms[z,3]
      edi_sub[y,50]<-river_kms[z,4]
      edi_sub[y,51]<-river_kms[z,5]
    }
  }
}
first_sjr<-edi_sub%>%
  filter(Station=="EZ2SJR"|Station=="EZ6SJR")%>%
  pull(Date)%>%
  min()

edi_2014<-filter(edi_sub,edi_sub$Date>=first_sjr)#first occurence of EZ SJR's

##### Section 2: calculate ez dist: include ez2 sjr pts if ez6 downstream of confluence #####
river_kms2<-river_kms%>%
  add_column(month_yr=format((river_kms$SampleDate), "%Y-%m"))
river_kms2<-river_kms2%>%
  add_column(month_count=table(river_kms2$month_yr)[river_kms2$month_yr])#=3 if only three datapoints for that month -> potentially downstream

#checks that all dates with 3 EZ points have an EZ2SJR pt; should be TRUE
nrow(filter(river_kms2,month_count==3&EZStation=="EZ2SJR"))==nrow(filter(river_kms2,month_count==3))/3
filter(river_kms2,month_count==3&EZStation=="EZ6SJR")# looks for EZ6SJR points in months with 3pts; should be 0 rows

ez_lengths2<-data.frame(matrix(nrow=300,ncol=5))
colnames(ez_lengths2) = c("name", "date", "len_km","upstr_km","downstr_km")
count=0
row=1
for (x in 1:nrow(river_kms2)) {
  if(x<nrow(river_kms2)){
    for (i in (x+1):nrow(river_kms2)){
      if(river_kms2[x,2]==river_kms2[i,2]){#if date is the same on both
        count=count+1
        ez_lengths2[row,1:5]<-c(river_kms2[x,1],river_kms2[x,2],as.numeric(abs(river_kms2[x,3]-river_kms2[i,3])),
                                max(river_kms2[x,3],river_kms2[i,3]),min(river_kms2[x,3],river_kms2[i,3]))
        row=row+1
      }
    }
    if(river_kms2[x,5]==3&river_kms2[x,1]=="EZ2SJR"){#for case where ez6 is below confluence, but ez2/2sjr are upstream
      ez6_down_conf<-filter(river_kms2,river_kms2$month_yr==river_kms2[x,4]&river_kms2$EZStation=="EZ6")#gives ez6 val from same month
      ez_lengths2[row,1:5]<-c(river_kms2[x,1],river_kms2[x,2],as.numeric(abs(river_kms2[x,3]-ez6_down_conf[3])),
                              river_kms2[x,3],ez6_down_conf[3])
      count=count+1
      row=row+1
      rm(ez6_down_conf)
    }
  }
}
#it works legooo; now 13 more rows than first version

ez_lengths2$len_km<-as.numeric(as.character(ez_lengths2$len_km))
ez_lengths2$date<-as.Date(as.numeric(ez_lengths2$date))
# ez_lengths2%>%
#   arrange(desc(len_km))
#write.csv(ez_lengths2,"ez_lengths_all_1April2022.csv")

##### Plotting EZ Dist figures (clean) #####
ez_lengths2<-read.csv("Outputs/ez_lengths_all_1April2022.csv")
#coloring by month
ggplot(data=ez_lengths2,aes(x=date,y=len_km))+
  geom_point(aes(color=month(date)))+
  labs(color="Month",x="Date",y="EZ Length")+
  theme(legend.position = "top")+
  scale_color_gradientn(colors = rainbow(5),
                        breaks=c(3,6,9,12),labels=c("Mar","Jun","Sep","Dec"))+
  theme(axis.title=element_text(size=12),
        axis.text=element_text(size=11),
        legend.text = element_text(size=11))

date_length_scat<-function(dataset,x_var,y_var,color_var,color_lab,midp){
  ggplot(data=dataset,aes(x=.data[[x_var]],y=.data[[y_var]]))+
    geom_point(aes(color=round(as.numeric(.data[[color_var]]),digits=0)))+
    labs(color=color_lab,x="Date",y="EZ Length")+
    scale_color_gradient2(midpoint = midp, low = "blue", mid = "white",
                          high = "red")+
    #breaks=c(3,6,9,12),labels=c("Mar","Jun","Sep","Dec"))+
    theme(legend.position = "top")+
    theme(axis.title=element_text(size=12),
          axis.text=element_text(size=11),
          legend.text = element_text(size=11))
}
up_col<-date_length_scat(ez_lengths2,"date","len_km","upstr_km","Upstream EZ km",70)
down_col<-date_length_scat(ez_lengths2,"date","len_km","downstr_km","Downstream EZ km",60)
grid.arrange(up_col, down_col, ncol=2)

# upstr/downstr scatter vs length

dist_len_scat<-function(dataset,dist_data,y_var,xlab){
  ggplot(data=dataset,aes(x=round(as.numeric(.data[[dist_data]]),digits=2),y=.data[[y_var]]))+
    geom_point()+
    geom_smooth(method = "lm")+
    labs(x=xlab,y="EZ Length")+
    theme(axis.title=element_text(size=12),
          axis.text=element_text(size=11),
          legend.text = element_text(size=11))
}
#par(mfrow=c(1,2))
up<-dist_len_scat(ez_lengths2,"upstr_km","len_km","Upstream EZ Distance")
down<-dist_len_scat(ez_lengths2,"downstr_km","len_km","Downstream EZ Distance")
grid.arrange(up, down, ncol=2)

#monthly boxplot
month_box<-function(dataset,x_var,y_var,xlab,ylab){
  ggplot(data=dataset,aes(x=month(.data[[x_var]]),y=.data[[y_var]]))+
    geom_boxplot(aes(group=month(.data[[x_var]])))+
    labs(x=xlab,y=ylab)+
    #scale_x_discrete(limits=factor(1,12))+
    scale_x_continuous(breaks=c(3,6,9,12),
                       labels=c("Mar","Jun","Sep","Dec"))+
    theme(axis.title=element_text(size=12),
          axis.text=element_text(size=11))
}
ez_month<-month_box(ez_lengths2,"date","len_km","Month","EZ Length")
ez_month
#eventually: subset to post-2014 data (not as needed for lengths tho, bc not comparing by location)

##### Usable ANOVAs #####
#this section is messy, but it's hard to be concise when running multiple single anova's
#can repeat this bit to get pvalues and Tukey pvalues for all our response variables, or load ANOVA_EZ-pvals_21Jan2022.csv

#Secchi
edi_2014<-arrange(edi_2014,Date)
lm_secchi<-lm(log10(Secchi)~location,data=edi_2014)#make a model; easier to do anovas this way
#also make sure for future lag steps that its arranged chronologically
par(mfrow = c(1, 1))
pacf(residuals(lm_secchi),na.action=na.pass)# auto correl fn; above blue bar significant; auto correl to self
print(stats::Box.test(residuals(lm_secchi), type = 'Ljung'))#signif?; need to add lag term
#if test fails: need to add lag term; auto correl model; add number that are over
edi_2014 <- edi_2014 %>%
  group_by(location) %>%
  mutate(Secchi_onelag = dplyr::lag(log10(Secchi), n = 1, default = NA),
         Secchi_twolag = dplyr::lag(log10(Secchi), n = 2, default = NA)) %>%
  ungroup()
lm_secchi<-lm(log10(Secchi)~location+Secchi_onelag,data=edi_2014)#run new model
#check new residuals
par(mfrow = c(1, 1))
pacf(residuals(lm_secchi),na.action=na.pass)# 
print(stats::Box.test(residuals(lm_secchi), type = 'Ljung'))#no longer signif
#check normality
plot(lm_secchi)#looks okay; i'd say good to use
aov_secchi <- Anova(lm_secchi, type = 'II', test.statistic = 'F')
print(aov_secchi)#signif
emm <- emmeans(lm_secchi, specs = pairwise ~ location~location, adjust = 'Tukey')
print(emm)#all three signif
summary(emm$contrasts)$p.value
#add pvalues to table
pvals<-data.frame(matrix(nrow=11,ncol=4))
colnames(pvals) = c("overall","join-sac","join-sjr","sac-sjr")
rownames(pvals)=c("Secchi","DissNitrateNitrite","TurbiditySurface","TurbidityBottom","pHSurface","pHBottom","Chla","Depth","DissOrthophos","DissAmmonia","DissSilica")
pval_fill<-function(var,aov_fn,emm_fn){
  pvals[var,1]<<-aov_fn$"Pr(>F)"[[1]]
  pvals[var,2:4]<<-summary(emm_fn$contrasts)$p.value
}
pval_fill("Secchi",aov_secchi,emm)

#trying NitrateNitrite
edi_2014<-arrange(edi_2014,Date)
lm_nit<-lm((DissNitrateNitrite)~location+Nitrate_onelag,data=edi_2014)#make a model; easier to do anovas this way
pacf(residuals(lm_nit),na.action=na.pass)# add first lag
print(stats::Box.test(residuals(lm_nit), type = 'Ljung'))#not signif w one lag, no log
edi_2014 <- edi_2014 %>%
  group_by(location) %>%
  mutate(Nitrate_onelag = dplyr::lag((DissNitrateNitrite), n = 1, default = NA),
         Nitrate_twolag = dplyr::lag((DissNitrateNitrite), n = 2, default = NA))%>%
  ungroup()
pacf(residuals(lm_nit),na.action=na.pass)# works best w only one lag; maybe always start w just one?
print(stats::Box.test(residuals(lm_nit), type = 'Ljung'))#better, still slightly signif
plot(lm_nit)#QQ plot not great -> log transform
leveneTest((DissNitrateNitrite)~location,data=edi_2014)#not signif, either w or w/o log transform
aov_nit <- Anova(lm_nit, type = 'II', test.statistic = 'F')
print(aov_nit)#not signif using 1 lag step
shapiro.test(x=residuals(object=aov(log10(DissNitrateNitrite)~location+Nitrate_onelag,data=edi_2014)))#still significant; but not signif when no log transform?? wtf??
kruskal.test((DissNitrateNitrite)~location,data=edi_2014)#now this is significant, idk how to interpret.....
emm_nit <- emmeans(lm_nit, specs = pairwise ~ location~location, adjust = 'Tukey')
print(emm_nit)#not signif
pval_fill("DissNitrateNitrite",aov_nit,emm_nit)
#results non-significant when no log transform (which we shouldnt use anyway) and one lag step (which messes up QQ plot)
#but significant for location (and specifically for joined-sac) when not using the one lag step; 
#lets keep using the lag, since that accounts for some of auto-correl, but just keep an eye out eh?

#TurbiditySurface
edi_2014<-arrange(edi_2014,Date)
lm_turb_s<-lm((TurbiditySurface)~location,data=edi_2014)
plot(lm_turb_s)#bad qq ->retry w log
lm_turb_s<-lm(log10(TurbiditySurface)~location,data=edi_2014)
plot(lm_turb_s)#good enough
boxplot(log10(TurbiditySurface)~location,data=edi_2014)#looks meh
leveneTest(log10(TurbiditySurface)~location,data=edi_2014)#signif; bleh
hist(log10(edi_2014$TurbiditySurface))
shapiro.test(x=residuals(object=aov(log10(TurbiditySurface)~location,data=edi_2014)))#slightly signif w log and no lag step
pacf(residuals(lm_turb_s),na.action=na.pass)# add first lag
print(stats::Box.test(residuals(lm_turb_s), type = 'Ljung'))#not signif w one lag, non-log; also not signif w one lag, log
edi_2014 <- edi_2014 %>%
  group_by(location) %>%
  mutate(turb_s_onelag = dplyr::lag(log10(TurbiditySurface), n = 1, default = NA),
         turb_s_twolag = dplyr::lag(log10(TurbiditySurface), n = 2, default = NA))%>%
  ungroup()
lm_turb_s<-lm(log10(TurbiditySurface)~location+turb_s_onelag,data=edi_2014)
pacf(residuals(lm_turb_s),na.action=na.pass)# add first lag
print(stats::Box.test(residuals(lm_turb_s), type = 'Ljung'))#not signif w one lag, log
plot(lm_turb_s)#looks worse
shapiro.test(x=residuals(object=aov(log10(TurbiditySurface)~location+turb_s_onelag,data=edi_2014)))#signif w/o log
aov_turb_s <- Anova(lm_turb_s, type = 'II', test.statistic = 'F')
print(aov_turb_s)#quite significant
emm_turb_s <- emmeans(lm_turb_s, specs = pairwise ~ location~location, adjust = 'Tukey')
print(emm_turb_s)#all signif
pval_fill("TurbiditySurface",aov_turb_s,emm_turb_s)
#same sh*t as above; log plot looks better, but not if we add in the needed lag step; proceed assuming it to be normal; 
#proceed as is bc just a few outlier points and anova's account for them pretty well

#2 region surface turbidity anova (what we used for IEP poster presentation)
edi_2014_2loc<-filter(edi_2014,location=="sac"|location=="sjr")
edi_2014_2loc<-arrange(edi_2014_2loc,Date)
lm_turb_s<-lm((TurbiditySurface)~location,data=edi_2014_2loc)
plot(lm_turb_s)#bad qq ->retry w log
lm_turb_s<-lm(log10(TurbiditySurface)~location,data=edi_2014_2loc)
plot(lm_turb_s)#good enough
boxplot(log10(TurbiditySurface)~location,data=edi_2014_2loc)#looks fine
leveneTest(log10(TurbiditySurface)~location,data=edi_2014_2loc)#not signif
hist(log10(edi_2014_2loc$TurbiditySurface))#pretty norm
shapiro.test(x=residuals(object=aov(log10(TurbiditySurface)~location,data=edi_2014_2loc)))#not signif
pacf(residuals(lm_turb_s),na.action=na.pass)# add first lag
print(stats::Box.test(residuals(lm_turb_s), type = 'Ljung'))
edi_2014_2loc <- edi_2014_2loc %>%
  group_by(location) %>%
  mutate(turb_s_onelag = dplyr::lag(log10(TurbiditySurface), n = 1, default = NA))%>%
  ungroup()
lm_turb_s<-lm(log10(TurbiditySurface)~location+turb_s_onelag,data=edi_2014_2loc)
pacf(residuals(lm_turb_s),na.action=na.pass)# add first lag
print(stats::Box.test(residuals(lm_turb_s), type = 'Ljung'))#not signif w one lag, log
plot(lm_turb_s)#looks alright
shapiro.test(x=residuals(object=aov(log10(TurbiditySurface)~location+turb_s_onelag,data=edi_2014_2loc)))#not signif
aov_turb_s <- Anova(lm_turb_s, type = 'II', test.statistic = 'F')
print(aov_turb_s)#quite significant
#ANOVA p value calculated with only Sac and SJR: 0.006167494

#TurbidityBottom
edi_2014<-arrange(edi_2014,Date)
lm_turb_b<-lm(log10(TurbidityBottom)~location+turb_b_onelag,data=edi_2014)
plot(lm_turb_b)
hist(log10(edi_2014$TurbidityBottom))#add in histogram; looks good enough to me
shapiro.test(x=residuals(object=aov(log10(TurbidityBottom)~location+turb_b_onelag,data=edi_2014)))#signif w/o log; still signif w it; but not signif w one lag legoo
#add in boxplots 4 variance
pacf(residuals(lm_turb_b),na.action=na.pass)# add first lag
print(stats::Box.test(residuals(lm_turb_b), type = 'Ljung'))#not signif w one lag, log
edi_2014 <- edi_2014 %>%
  group_by(location) %>%
  mutate(turb_b_onelag = dplyr::lag(log10(TurbidityBottom), n = 1, default = NA))%>%
  ungroup()
leveneTest(log10(TurbidityBottom)~location,data=edi_2014)#signif, but idk doesn't include lag stop
plot(aov(log10(TurbidityBottom)~location+turb_b_onelag,data=edi_2014),1)#idk if good or bad, going to proceed as is for now
aov_turb_b <- Anova(lm_turb_b, type = 'II', test.statistic = 'F')
print(aov_turb_b)#quite significant
emm_turb_b <- emmeans(lm_turb_b, specs = pairwise ~ location~location, adjust = 'Tukey')
print(emm_turb_b)#joined vs sac can't signif explain differences in variance, but rest can
pval_fill("TurbidityBottom",aov_turb_b,emm_turb_b)

#pHSurface
edi_2014<-arrange(edi_2014,Date)
lm_ph_s<-lm((pHSurface)~location,data=edi_2014)
hist((edi_2014$pHSurface))
plot(lm_ph_s)#already log scale-> looks good
shapiro.test(x=residuals(object=aov((pHSurface)~location,data=edi_2014)))#slightly signif but i think its aight
boxplot((pHSurface)~location,data=edi_2014)#looks pretttty good
leveneTest((pHSurface)~location,data=edi_2014)#not signif, noice
pacf(residuals(lm_ph_s),na.action=na.pass)# add first, maybe second
print(stats::Box.test(residuals(lm_ph_s), type = 'Ljung'))#v signif
edi_2014 <- edi_2014 %>%
  group_by(location) %>%
  mutate(ph_s_onelag = dplyr::lag((pHSurface), n = 1, default = NA))%>%
  ungroup()
lm_ph_s<-lm((pHSurface)~location+ph_s_onelag,data=edi_2014)
pacf(residuals(lm_ph_s),na.action=na.pass)
print(stats::Box.test(residuals(lm_ph_s), type = 'Ljung'))#not signif w just one
plot(lm_ph_s,2)
shapiro.test(x=residuals(object=aov((pHSurface)~location+ph_s_onelag,data=edi_2014)))#not signif anymore; coool
aov_ph_s <- Anova(lm_ph_s, type = 'II', test.statistic = 'F')
print(aov_ph_s)#not signif ya bish
emm_ph_s <- emmeans(lm_ph_s, specs = pairwise ~ location~location, adjust = 'Tukey')
print(emm_ph_s)#none signif boiii
pval_fill("pHSurface",aov_ph_s,emm_ph_s)

#way to visualize independence tests
init_tests <- function (vardep, varindep1, DATA) {
  x<-lm(paste(vardep, "~", varindep1), data = DATA)
  plot(x,2)
  pacf(residuals(x),na.action=na.pass)
  print(stats::Box.test(residuals(x), type = 'Ljung'))
}

#pHBottom
init_tests("pHBottom","location",edi_2014)#qq aight, but need a lag case
hist((edi_2014$pHBottom))
boxplot((pHBottom)~location,data=edi_2014)
shapiro.test(x=residuals(object=aov((pHBottom)~location,data=edi_2014)))#signif
leveneTest((pHBottom)~location,data=edi_2014)#not signif, noice
edi_2014 <- edi_2014 %>%
  group_by(location) %>%
  mutate(ph_b_onelag = dplyr::lag((pHBottom), n = 1, default = NA))%>%
  ungroup()
lm_ph_b<-lm((pHBottom)~location+ph_b_onelag,data=edi_2014)
pacf(residuals(lm_ph_b),na.action=na.pass)
print(stats::Box.test(residuals(lm_ph_b), type = 'Ljung'))#not signif w just one
plot(lm_ph_b)
shapiro.test(x=residuals(object=aov((pHBottom)~location+ph_b_onelag,data=edi_2014)))#even more significant now, but just a couple outliers
aov_ph_b <- Anova(lm_ph_b, type = 'II', test.statistic = 'F')
print(aov_ph_b)#signif
emm_ph_b <- emmeans(lm_ph_b, specs = pairwise ~ location~location, adjust = 'Tukey')
print(emm_ph_b)#no indiv comps signif
pval_fill("pHBottom",aov_ph_b,emm_ph_b)

#Chla
init_tests("Chla","location",edi_2014)#qq aight, but need a lag case
hist((edi_2014$Chla))
boxplot((Chla)~location,data=edi_2014)
shapiro.test(x=residuals(object=aov((Chla)~location,data=edi_2014)))#signif, add log
leveneTest((Chla)~location,data=edi_2014)#not signif
lm_chla<-lm(log10(Chla)~location,data=edi_2014)
plot(lm_chla,1:2)#still looks bad after log transform, what 2 do?
shapiro.test(x=residuals(object=aov(log10(Chla)~location,data=edi_2014)))#signif
edi_2014 <- edi_2014 %>%
  group_by(location) %>%
  mutate(chla_onelag = dplyr::lag(log10(Chla), n = 1, default = NA),
         chla_twolag = dplyr::lag(log10(Chla), n = 2, default = NA))%>%
  ungroup()
lm_chla<-lm(log10(Chla)~location+chla_onelag+chla_twolag,data=edi_2014)
pacf(residuals(lm_chla),na.action=na.pass)
print(stats::Box.test(residuals(lm_chla), type = 'Ljung'))#still signif...
shapiro.test(x=residuals(object=aov(log10(Chla)~location+chla_onelag+chla_twolag,data=edi_2014)))#still signif
aov_chla <- Anova(lm_chla, type = 'II', test.statistic = 'F')
print(aov_chla)#very NOT signif
emm_chla <- emmeans(lm_chla, specs = pairwise ~ location~location, adjust = 'Tukey')
print(emm_chla)#very NOT signif
pval_fill("Chla",aov_chla,emm_chla)
kruskal.test(log10(Chla)~location,data=edi_2014)#as a check, this is verrry insignif

auto_corr_vis<-function(lm_fn){
  pacf(residuals(lm_fn),na.action=na.pass)
  print(stats::Box.test(residuals(lm_fn), type = 'Ljung'))
  plot(lm_fn,2)
  print(shapiro.test(residuals(lm_fn)))
}
#function to print anova, tukey values
end_game_vis<-function(lm_fn){
  print(Anova(lm_fn, type = 'II', test.statistic = 'F'))
  print(emmeans(lm_fn, specs = pairwise ~ location~location, adjust = 'Tukey'))
}
#run final anova and tukey values and fill into pvals table
pval_fill_end<-function(var,lm_fn){
  aov_fn<-Anova(lm_fn, type = 'II', test.statistic = 'F')
  emm_fn<-emmeans(lm_fn, specs = pairwise ~ location~location, adjust = 'Tukey')
  pvals[var,1]<<-aov_fn$"Pr(>F)"[[1]]
  pvals[var,2:4]<<-summary(emm_fn$contrasts)$p.value
}

#Depth
init_tests("Depth","location",edi_2014)#qq bad, may not need lag
hist((edi_2014$Depth))
boxplot((Depth)~location,data=edi_2014)
shapiro.test(x=residuals(object=aov((Depth)~location,data=edi_2014)))#signif
leveneTest((Depth)~location,data=edi_2014)# signif
lm_depth<-lm(log10(Depth)~location,data=edi_2014)
plot(lm_depth,1:2)# looks worse after log transform; proceed w/o log tranf, just try a kruskals after
shapiro.test(x=residuals(object=aov(log10(Depth)~location,data=edi_2014)))#signif
lm_depth<-lm((Depth)~location,data=edi_2014)
auto_corr_vis(lm_depth)#try one lag
edi_2014 <- edi_2014 %>%
  group_by(location) %>%
  mutate(depth_onelag = dplyr::lag((Depth), n = 1, default = NA))%>%
  ungroup()
lm_depth<-lm((Depth)~location+depth_onelag,data=edi_2014)
auto_corr_vis(lm_depth)#no autocorr
shapiro.test(x=residuals(object=aov((Depth)~location+depth_onelag,data=edi_2014)))#bad still, oh well
end_game_vis(lm_depth)#signif for join-sac,sac-sjr
pval_fill_end("Depth",lm_depth)
kruskal.test((Depth)~location,data=edi_2014)#as a check, this is quite signif too

#visualize normality, heteroskedasticity tests
norm_vis<-function(var){
  hist(var)
  boxplot((var)~location,data=edi_2014)
  print(shapiro.test(x=residuals(object=aov((var)~location,data=edi_2014))))#signif
  print(leveneTest((var)~location,data=edi_2014))# signif
  plot(lm(var~location,data=edi_2014),2)
}

#DissOrthoPhos
init_tests("DissOrthophos","location",edi_2014)#needs one, maybe two lags
norm_vis((edi_2014$DissOrthophos))
norm_vis(log10(edi_2014$DissOrthophos))#lets go with log10, tho still pretty bad
lm_orthophos<-lm(log10(DissOrthophos)~location,data=edi_2014)
auto_corr_vis(lm_orthophos)
edi_2014 <- edi_2014 %>%
  group_by(location) %>%
  mutate(ortho_onelag = dplyr::lag(log10(DissOrthophos), n = 1, default = NA))%>%
  ungroup()
lm_orthophos<-lm(log10(DissOrthophos)~location+ortho_onelag,data=edi_2014)
auto_corr_vis(lm_orthophos)#no autocorr, but qq and shapiro's look bad; carry on?
end_game_vis(lm_orthophos)#joined vs sac, joined-sjr signif
pval_fill_end("DissOrthophos",lm_orthophos)
kruskal.test((DissOrthophos)~location,data=edi_2014)#as a check, this is quite signif too

#DissAmmonia
#init_tests("DissAmmonia","location",edi_2014)#needs one, maybe two lags
norm_vis((edi_2014$DissAmmonia))
norm_vis(log10(edi_2014$DissAmmonia))#now not signif
lm_ammon<-lm(log10(DissAmmonia)~location,data=edi_2014)
auto_corr_vis(lm_ammon)#add first, maybe second
edi_2014 <- edi_2014 %>%
  group_by(location) %>%
  mutate(ammon_onelag = dplyr::lag(log10(DissAmmonia), n = 1, default = NA),
         ammon_twolag = dplyr::lag(log10(DissAmmonia), n = 2, default = NA))%>%
  ungroup()
lm_ammon<-lm(log10(DissAmmonia)~location+ammon_onelag,data=edi_2014)
auto_corr_vis(lm_ammon)#keep as is, looks worse w two lags; normally distr
end_game_vis(lm_ammon)#not signif
pval_fill_end("DissAmmonia",lm_ammon)

#DissSilica
norm_vis((edi_2014$DissSilica))#pretty bad
norm_vis(log10(edi_2014$DissSilica))#still bad, esp a few outliers
lm_sil<-lm(log10(DissSilica)~location,data=edi_2014)
auto_corr_vis(lm_sil)#ADD FIRST
edi_2014 <- edi_2014 %>%
  group_by(location) %>%
  mutate(sil_onelag = dplyr::lag(log10(DissSilica), n = 1, default = NA),
         sil_twolag = dplyr::lag(log10(DissSilica), n = 2, default = NA))%>%
  ungroup()
lm_sil<-lm(log10(DissSilica)~location+sil_onelag,data=edi_2014)
auto_corr_vis(lm_sil)#best w one; normality still bad -> try kruskal's at end
end_game_vis(lm_sil)#not signif (tho sac-sjr close)
pval_fill_end("DissSilica",lm_sil)
kruskal.test((DissSilica)~location,data=edi_2014)#welp its significant here...
dunn.test((edi_2014$DissSilica),edi_2014$location)#non-parametric anova test.... should we pursue further??? (comes up as signif) idk dont account for autocorr tho

#pvals_final<-pvals[c(1:8,10:12),]
#write.csv(pvals_final,"ANOVA_EZ-pvals_21Jan2022.csv")

##### Box plotting (clean) #####
#visualizing
locn_box14<-function(analyte){
  ggplot(data=edi_2014,aes(x=.data[["loc_num"]],y=(.data[[analyte]])))+
    geom_boxplot(aes(group=.data[["loc_num"]]),outlier.shape = NA)+
    labs(x="Location",y=analyte)+
    scale_x_continuous(breaks=c(1,2,3),labels=c("SJR","Sac","Joined"))+
    scale_y_continuous(limits = quantile(edi_2014[analyte],na.rm=T, c(0.1, 0.9)))+#fix this
    #coord_cartesian(ylim = quantile(.data[[analyte]],na.rm=T, c(0.1, 0.9)))+#y u no work???
    #coord_cartesian(ylim=c(0,10))+
    theme(axis.title=element_text(size=12),
          axis.text=element_text(size=11))
}
grid.arrange(locn_box14("Secchi"),locn_box14("TurbidityBottom"),locn_box14("TurbiditySurface"),locn_box14("Depth"),
             locn_box14("pHBottom"),locn_box14("pHSurface"),locn_box14("WTBottom"),locn_box14("WTSurface"),
             locn_box14("DOpercentBottom"),locn_box14("DOBottom"),locn_box14("DOpercentSurface"),locn_box14("DOSurface"),ncol=4)
bryte<-colnames(edi_2014[6:23])
grid.arrange(locn_box14(bryte[2]),locn_box14(bryte[3]),locn_box14(bryte[4]),
             locn_box14(bryte[5]),locn_box14(bryte[1]),locn_box14(bryte[6]),
             locn_box14(bryte[7]),locn_box14(bryte[11]),locn_box14(bryte[13]),ncol=3)
grid.arrange(locn_box14(bryte[8]),locn_box14(bryte[9]),locn_box14(bryte[10]),
             locn_box14(bryte[14]),locn_box14(bryte[15]),locn_box14(bryte[16]),
             locn_box14(bryte[12]),locn_box14(bryte[17]),locn_box14(bryte[18]),ncol=3)

#final poster version, using orange/yellow color scheme
ggplot(data=edi_2014_2loc,aes(x=location,y=log10(TurbiditySurface),fill=location))+
  geom_boxplot(aes(group=location),lwd=1.3)+#removed this line: ,outlier.shape = NA (removes outlier pts)
  scale_x_discrete(breaks=c("sac","sjr"),labels=c("Sacramento","San Joaquin"))+
  labs(x="Region",y="log10(SurfaceTurbidity)")+
  scale_fill_manual(values = c("#ec7014", "#fec44f"))+
  #annotate("text", x=1, y=1.75, label= paste("ANOVA: p=", signif(pvals[3,5],3)),size=9) +#location and input diff for zoops
  theme_classic()+
  theme(axis.title=element_text(size=23),
        axis.text=element_text(size=22,color="black"),
        legend.position = "none")

#for reporting values on poster
sac<-filter(edi_2014_2loc,location=="sac")
sjr<-filter(edi_2014_2loc,location=="sjr")
median(sac$TurbiditySurface)
median(sjr$TurbiditySurface)
mean(sac$TurbiditySurface)
mean(sjr$TurbiditySurface)
sd(sac$TurbiditySurface)
sd(sjr$TurbiditySurface)

#3 region plots (not using)
ggplot(data=edi_2014,aes(x=location,y=log10(TurbiditySurface),fill=location))+
  geom_boxplot(aes(group=location),outlier.shape = NA,lwd=1.3)+
  scale_x_discrete(breaks=c("joined","sac","sjr"),labels=c("Confluence","Sacramento","San Joaquin"))+
  labs(x="Region",y="log10(SurfaceTurbidity)")+
  scale_fill_manual(values = c("#993404", "#ec7014", "#fec44f"))+
  #annotate("text", x=1, y=1.75, label= paste("ANOVA: p=", signif(pvals[3,5],3)),size=9) +#location and input diff for zoops
  theme_classic()+
  theme(axis.title=element_text(size=21),
        axis.text=element_text(size=20),
        legend.position = "none")

##### Plotting maps (clean) #####

ez2_sjr_edi<-filter(edi_sub,Station=="EZ2SJR")
ez6_sjr_edi<-filter(edi_sub,Station=="EZ6SJR")
ez2_edi<-filter(edi_sub,Station=="EZ2")
ez6_edi<-filter(edi_sub,Station=="EZ6")

theme_set(theme_bw())
ez_mapper<-function(dataset,xlim,ylim,breaks,title){
  ggplot(WW_Delta)+
    geom_sf()+
    geom_point(data=dataset,aes(x=west_long,y=north_lat,color=Date),size=3.5)+
    coord_sf(xlim = xlim,ylim = ylim,expand=F)+#closeup
    scale_color_viridis_c(breaks = as.numeric(dataset$Date[breaks]),labels=dataset$Date[breaks])+
    xlab("Longitude")+
    ylab("Latitude")+
    ggtitle(title)+
    theme(axis.title=element_text(size=12),
          axis.text=element_text(size=11),
          legend.text=element_text(size=10),
          legend.title = element_text(size=11),
          plot.title = element_text(hjust = 0.5,size=15,face="bold"))
}
ez2sjr_plot<-ez_mapper(ez2_sjr_edi,c(-121.85,-121.6),c(38,38.15),c(5,15,25,35,44),"EZ2-SJR")
ez6sjr_plot<-ez_mapper(ez6_sjr_edi,c(-121.85,-121.6),c(38,38.15),c(1,15,19,29),"EZ6-SJR")
grid.arrange(ez2sjr_plot,ez6sjr_plot,ncol=1)
ez2_plot<-ez_mapper(ez2_edi,c(-122.3,-121.65),c(38,38.2),c(5,54,103,152,200),"EZ2")
ez6_plot<-ez_mapper(ez6_edi,c(-122.3,-121.65),c(38,38.2),c(5,54,103,152,200),"EZ6")
grid.arrange(ez2_plot,ez6_plot,ncol=1)

##### area map for poster (clean) #####
basemap=get_stamenmap(bbox=c(left=-122.6,
                             right=-121.0, bottom=37.4, top=38.7),
                      maptype="terrain")
ww2<-WW_Delta
top2<-c("SACRAMENTO RIVER","SAN JOAQUIN RIVER")
ww2$HNAME[!(ww2$HNAME %in% top2)]="zzz"
ww2$HNAME2<-factor(ww2$HNAME,labels=c("Sacramento River","San Joaquin River","Other Waterway"))
colormap<-ggmap(basemap, darken = c(.5, "white"))+
  geom_sf(data = ww2, inherit.aes = F, aes(fill = HNAME2))+
  scale_fill_manual(#breaks=c("Sacramento River","San Joaquin River"),
    values=c("#ec7014","#fec44f","#d1edf2"),
    na.translate=F, drop=F)+
  coord_sf(xlim = c(-121.95,-121.62),ylim = c(37.99,38.16),expand=F)+
  scale_color_discrete(guide = "none") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.text=element_text(size=14),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5,size=15,face="bold"),
        panel.background = element_rect(fill = NA,color="black"),#uncomment for white background map
        legend.position = "top")

c<-data.frame(lat=c(37.804363, 38.590576),
              lon=c(-122.271111, -121.489906))
g_col<-"#bae4bc"#best green
b_col<-"#0868ac"#fav blue
insetmap<-ggplot(WW_Delta)+
  geom_sf(fill=b_col,lwd=0.1,color=b_col)+#blue
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_rect(fill=g_col),
        #panel.background = element_rect(fill=g_col,color="black"), uncomment forwhite back trial
        #plot.background = element_blank(),#uncomment for white background
        panel.grid.major = element_blank(),
        panel.border = element_blank())+
  coord_sf(xlim = c(-122.55,-121),ylim = c(37.45,38.7),expand=F)+
  annotate(geom = "rect", ymax = 38.15, ymin = 37.99, xmax = -121.62, xmin = -121.95, colour = "red", fill = NA,lwd=2)+
  geom_point(data=c, aes(x=lon, y=lat), color="black", size=5)+
  annotate("text",x=-121.85,y=37.76,label="Oakland",size=5)+
  annotate("text",x=-122.05,y=38.63,label="Sacramento",size=5)
vp_inset <- grid::viewport(width = 0.35, height = 0.35, x = .146, y = .74)
print(colormap)
print(insetmap, vp = vp_inset)
