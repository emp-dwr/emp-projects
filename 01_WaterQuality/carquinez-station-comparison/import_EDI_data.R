#Importing data from EDI
# Package ID: edi.458.3 Cataloging System:https://pasta.edirepository.org.
# Data set title: Interagency Ecological Program: Discrete water quality monitoring in the Sacramento-San Joaquin Bay-Delta, collected by the Environmental Monitoring Program, 1975-2019..
# Data set creator:   Interagency Ecological Program (IEP) -  
# Data set creator:  Morgan Martinez - California Department of Water Resources 
# Data set creator:  Jenna Rinde - California Department of Water Resources 
# Data set creator:  Theodore Flynn - California Department of Water Resources 
# Data set creator:  Sarah Lesmeister - California Department of Water Resources 
# Contact:  Morgan Martinez -  California Department of Water Resources  - morgan.martinez@water.ca.gov
# Contact:  Sarah Lesmeister -  California Department of Water Resources  - sarah.lesmeister@water.ca.gov
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

import_EDI <- function() {
  inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/458/3/abedcf730c2490fde0237df58c897556" 
  infile1 <- tempfile()
  try(download.file(inUrl1,infile1,method="curl"))
  if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
  
  
  dt1 <-read.csv(infile1,header=F 
                 ,skip=1
                 ,sep=","  
                 ,quot='"' 
                 , col.names=c(
                   "Station",     
                   "Date",     
                   "Time",     
                   "SampleDescription",     
                   "FieldNotes",     
                   "Weather",     
                   "AirTemp",     
                   "WindVelocity",     
                   "WindDirection",     
                   "NorthLat",     
                   "WestLong",     
                   "Chla",     
                   "Pheophytin",     
                   "TotAlkalinity",     
                   "TotAmmonia",     
                   "DissAmmonia",     
                   "DissBromide",     
                   "DissCalcium",     
                   "TotChloride",     
                   "DissChloride",     
                   "DissNitrateNitrite",     
                   "DOC",     
                   "TOC",     
                   "DON",     
                   "TON",     
                   "DissOrthophos",     
                   "TotPhos",     
                   "DissSilica",     
                   "TDS",     
                   "TSS",     
                   "VSS",     
                   "TKN",     
                   "Depth",     
                   "Secchi",     
                   "Microcystis",     
                   "LightExtinction",     
                   "SpCndSurface",     
                   "DOSurface",     
                   "DOpercentSurface",     
                   "WTSurface",     
                   "TurbiditySurface",     
                   "pHSurface",     
                   "SpCndBottom",     
                   "DOBottom",     
                   "DOpercentBottom",     
                   "WTBottom",     
                   "TurbidityBottom",     
                   "pHBottom"    ), check.names=TRUE)
  
  unlink(infile1)
  
  # # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
  # 
  # if (class(dt1$Station)!="factor") dt1$Station<- as.factor(dt1$Station)                                   
  # # attempting to convert dt1$Date dateTime string to R date structure (date or POSIXct)                                
  # tmpDateFormat<-"%m/%d/%Y"
  # tmp1Date<-as.Date(dt1$Date,format=tmpDateFormat)
  # # Keep the new dates only if they all converted correctly
  # if(length(tmp1Date) == length(tmp1Date[!is.na(tmp1Date)])){dt1$Date <- tmp1Date } else {print("Date conversion failed for dt1$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
  # rm(tmpDateFormat,tmp1Date) 
  # if (class(dt1$SampleDescription)!="factor") dt1$SampleDescription<- as.factor(dt1$SampleDescription)
  # if (class(dt1$FieldNotes)!="factor") dt1$FieldNotes<- as.factor(dt1$FieldNotes)
  # if (class(dt1$Weather)!="factor") dt1$Weather<- as.factor(dt1$Weather)
  # if (class(dt1$AirTemp)=="factor") dt1$AirTemp <-as.numeric(levels(dt1$AirTemp))[as.integer(dt1$AirTemp) ]               
  # if (class(dt1$AirTemp)=="character") dt1$AirTemp <-as.numeric(dt1$AirTemp)
  # if (class(dt1$WindVelocity)=="factor") dt1$WindVelocity <-as.numeric(levels(dt1$WindVelocity))[as.integer(dt1$WindVelocity) ]               
  # if (class(dt1$WindVelocity)=="character") dt1$WindVelocity <-as.numeric(dt1$WindVelocity)
  # if (class(dt1$WindDirection)=="factor") dt1$WindDirection <-as.numeric(levels(dt1$WindDirection))[as.integer(dt1$WindDirection) ]               
  # if (class(dt1$WindDirection)=="character") dt1$WindDirection <-as.numeric(dt1$WindDirection)
  # if (class(dt1$NorthLat)=="factor") dt1$NorthLat <-as.numeric(levels(dt1$NorthLat))[as.integer(dt1$NorthLat) ]               
  # if (class(dt1$NorthLat)=="character") dt1$NorthLat <-as.numeric(dt1$NorthLat)
  # if (class(dt1$WestLong)=="factor") dt1$WestLong <-as.numeric(levels(dt1$WestLong))[as.integer(dt1$WestLong) ]               
  # if (class(dt1$WestLong)=="character") dt1$WestLong <-as.numeric(dt1$WestLong)
  # if (class(dt1$Chla)=="factor") dt1$Chla <-as.numeric(levels(dt1$Chla))[as.integer(dt1$Chla) ]               
  # if (class(dt1$Chla)=="character") dt1$Chla <-as.numeric(dt1$Chla)
  # if (class(dt1$Pheophytin)=="factor") dt1$Pheophytin <-as.numeric(levels(dt1$Pheophytin))[as.integer(dt1$Pheophytin) ]               
  # if (class(dt1$Pheophytin)=="character") dt1$Pheophytin <-as.numeric(dt1$Pheophytin)
  # if (class(dt1$TotAlkalinity)=="factor") dt1$TotAlkalinity <-as.numeric(levels(dt1$TotAlkalinity))[as.integer(dt1$TotAlkalinity) ]               
  # if (class(dt1$TotAlkalinity)=="character") dt1$TotAlkalinity <-as.numeric(dt1$TotAlkalinity)
  # if (class(dt1$TotAmmonia)=="factor") dt1$TotAmmonia <-as.numeric(levels(dt1$TotAmmonia))[as.integer(dt1$TotAmmonia) ]               
  # if (class(dt1$TotAmmonia)=="character") dt1$TotAmmonia <-as.numeric(dt1$TotAmmonia)
  # if (class(dt1$DissAmmonia)=="factor") dt1$DissAmmonia <-as.numeric(levels(dt1$DissAmmonia))[as.integer(dt1$DissAmmonia) ]               
  # if (class(dt1$DissAmmonia)=="character") dt1$DissAmmonia <-as.numeric(dt1$DissAmmonia)
  # if (class(dt1$DissBromide)=="factor") dt1$DissBromide <-as.numeric(levels(dt1$DissBromide))[as.integer(dt1$DissBromide) ]               
  # if (class(dt1$DissBromide)=="character") dt1$DissBromide <-as.numeric(dt1$DissBromide)
  # if (class(dt1$DissCalcium)=="factor") dt1$DissCalcium <-as.numeric(levels(dt1$DissCalcium))[as.integer(dt1$DissCalcium) ]               
  # if (class(dt1$DissCalcium)=="character") dt1$DissCalcium <-as.numeric(dt1$DissCalcium)
  # if (class(dt1$TotChloride)=="factor") dt1$TotChloride <-as.numeric(levels(dt1$TotChloride))[as.integer(dt1$TotChloride) ]               
  # if (class(dt1$TotChloride)=="character") dt1$TotChloride <-as.numeric(dt1$TotChloride)
  # if (class(dt1$DissChloride)=="factor") dt1$DissChloride <-as.numeric(levels(dt1$DissChloride))[as.integer(dt1$DissChloride) ]               
  # if (class(dt1$DissChloride)=="character") dt1$DissChloride <-as.numeric(dt1$DissChloride)
  # if (class(dt1$DissNitrateNitrite)=="factor") dt1$DissNitrateNitrite <-as.numeric(levels(dt1$DissNitrateNitrite))[as.integer(dt1$DissNitrateNitrite) ]               
  # if (class(dt1$DissNitrateNitrite)=="character") dt1$DissNitrateNitrite <-as.numeric(dt1$DissNitrateNitrite)
  # if (class(dt1$DOC)=="factor") dt1$DOC <-as.numeric(levels(dt1$DOC))[as.integer(dt1$DOC) ]               
  # if (class(dt1$DOC)=="character") dt1$DOC <-as.numeric(dt1$DOC)
  # if (class(dt1$TOC)=="factor") dt1$TOC <-as.numeric(levels(dt1$TOC))[as.integer(dt1$TOC) ]               
  # if (class(dt1$TOC)=="character") dt1$TOC <-as.numeric(dt1$TOC)
  # if (class(dt1$DON)=="factor") dt1$DON <-as.numeric(levels(dt1$DON))[as.integer(dt1$DON) ]               
  # if (class(dt1$DON)=="character") dt1$DON <-as.numeric(dt1$DON)
  # if (class(dt1$TON)=="factor") dt1$TON <-as.numeric(levels(dt1$TON))[as.integer(dt1$TON) ]               
  # if (class(dt1$TON)=="character") dt1$TON <-as.numeric(dt1$TON)
  # if (class(dt1$DissOrthophos)=="factor") dt1$DissOrthophos <-as.numeric(levels(dt1$DissOrthophos))[as.integer(dt1$DissOrthophos) ]               
  # if (class(dt1$DissOrthophos)=="character") dt1$DissOrthophos <-as.numeric(dt1$DissOrthophos)
  # if (class(dt1$TotPhos)=="factor") dt1$TotPhos <-as.numeric(levels(dt1$TotPhos))[as.integer(dt1$TotPhos) ]               
  # if (class(dt1$TotPhos)=="character") dt1$TotPhos <-as.numeric(dt1$TotPhos)
  # if (class(dt1$DissSilica)=="factor") dt1$DissSilica <-as.numeric(levels(dt1$DissSilica))[as.integer(dt1$DissSilica) ]               
  # if (class(dt1$DissSilica)=="character") dt1$DissSilica <-as.numeric(dt1$DissSilica)
  # if (class(dt1$TDS)=="factor") dt1$TDS <-as.numeric(levels(dt1$TDS))[as.integer(dt1$TDS) ]               
  # if (class(dt1$TDS)=="character") dt1$TDS <-as.numeric(dt1$TDS)
  # if (class(dt1$TSS)=="factor") dt1$TSS <-as.numeric(levels(dt1$TSS))[as.integer(dt1$TSS) ]               
  # if (class(dt1$TSS)=="character") dt1$TSS <-as.numeric(dt1$TSS)
  # if (class(dt1$VSS)=="factor") dt1$VSS <-as.numeric(levels(dt1$VSS))[as.integer(dt1$VSS) ]               
  # if (class(dt1$VSS)=="character") dt1$VSS <-as.numeric(dt1$VSS)
  # if (class(dt1$TKN)=="factor") dt1$TKN <-as.numeric(levels(dt1$TKN))[as.integer(dt1$TKN) ]               
  # if (class(dt1$TKN)=="character") dt1$TKN <-as.numeric(dt1$TKN)
  # if (class(dt1$Depth)=="factor") dt1$Depth <-as.numeric(levels(dt1$Depth))[as.integer(dt1$Depth) ]               
  # if (class(dt1$Depth)=="character") dt1$Depth <-as.numeric(dt1$Depth)
  # if (class(dt1$Secchi)=="factor") dt1$Secchi <-as.numeric(levels(dt1$Secchi))[as.integer(dt1$Secchi) ]               
  # if (class(dt1$Secchi)=="character") dt1$Secchi <-as.numeric(dt1$Secchi)
  # if (class(dt1$Microcystis)=="factor") dt1$Microcystis <-as.numeric(levels(dt1$Microcystis))[as.integer(dt1$Microcystis) ]               
  # if (class(dt1$Microcystis)=="character") dt1$Microcystis <-as.numeric(dt1$Microcystis)
  # if (class(dt1$LightExtinction)=="factor") dt1$LightExtinction <-as.numeric(levels(dt1$LightExtinction))[as.integer(dt1$LightExtinction) ]               
  # if (class(dt1$LightExtinction)=="character") dt1$LightExtinction <-as.numeric(dt1$LightExtinction)
  # if (class(dt1$SpCndSurface)=="factor") dt1$SpCndSurface <-as.numeric(levels(dt1$SpCndSurface))[as.integer(dt1$SpCndSurface) ]               
  # if (class(dt1$SpCndSurface)=="character") dt1$SpCndSurface <-as.numeric(dt1$SpCndSurface)
  # if (class(dt1$DOSurface)=="factor") dt1$DOSurface <-as.numeric(levels(dt1$DOSurface))[as.integer(dt1$DOSurface) ]               
  # if (class(dt1$DOSurface)=="character") dt1$DOSurface <-as.numeric(dt1$DOSurface)
  # if (class(dt1$DOpercentSurface)=="factor") dt1$DOpercentSurface <-as.numeric(levels(dt1$DOpercentSurface))[as.integer(dt1$DOpercentSurface) ]               
  # if (class(dt1$DOpercentSurface)=="character") dt1$DOpercentSurface <-as.numeric(dt1$DOpercentSurface)
  # if (class(dt1$WTSurface)=="factor") dt1$WTSurface <-as.numeric(levels(dt1$WTSurface))[as.integer(dt1$WTSurface) ]               
  # if (class(dt1$WTSurface)=="character") dt1$WTSurface <-as.numeric(dt1$WTSurface)
  # if (class(dt1$TurbiditySurface)!="factor") dt1$TurbiditySurface<- as.factor(dt1$TurbiditySurface)
  # if (class(dt1$pHSurface)=="factor") dt1$pHSurface <-as.numeric(levels(dt1$pHSurface))[as.integer(dt1$pHSurface) ]               
  # if (class(dt1$pHSurface)=="character") dt1$pHSurface <-as.numeric(dt1$pHSurface)
  # if (class(dt1$SpCndBottom)=="factor") dt1$SpCndBottom <-as.numeric(levels(dt1$SpCndBottom))[as.integer(dt1$SpCndBottom) ]               
  # if (class(dt1$SpCndBottom)=="character") dt1$SpCndBottom <-as.numeric(dt1$SpCndBottom)
  # if (class(dt1$DOBottom)=="factor") dt1$DOBottom <-as.numeric(levels(dt1$DOBottom))[as.integer(dt1$DOBottom) ]               
  # if (class(dt1$DOBottom)=="character") dt1$DOBottom <-as.numeric(dt1$DOBottom)
  # if (class(dt1$DOpercentBottom)=="factor") dt1$DOpercentBottom <-as.numeric(levels(dt1$DOpercentBottom))[as.integer(dt1$DOpercentBottom) ]               
  # if (class(dt1$DOpercentBottom)=="character") dt1$DOpercentBottom <-as.numeric(dt1$DOpercentBottom)
  # if (class(dt1$WTBottom)=="factor") dt1$WTBottom <-as.numeric(levels(dt1$WTBottom))[as.integer(dt1$WTBottom) ]               
  # if (class(dt1$WTBottom)=="character") dt1$WTBottom <-as.numeric(dt1$WTBottom)
  # if (class(dt1$TurbidityBottom)=="factor") dt1$TurbidityBottom <-as.numeric(levels(dt1$TurbidityBottom))[as.integer(dt1$TurbidityBottom) ]               
  # if (class(dt1$TurbidityBottom)=="character") dt1$TurbidityBottom <-as.numeric(dt1$TurbidityBottom)
  # if (class(dt1$pHBottom)=="factor") dt1$pHBottom <-as.numeric(levels(dt1$pHBottom))[as.integer(dt1$pHBottom) ]               
  # if (class(dt1$pHBottom)=="character") dt1$pHBottom <-as.numeric(dt1$pHBottom)
  # 
  # # Convert Missing Values to NA for non-dates
  # 
  # dt1$Station <- as.factor(ifelse((trimws(as.character(dt1$Station))==trimws("NA")),NA,as.character(dt1$Station)))
  # dt1$Station <- as.factor(ifelse((trimws(as.character(dt1$Station))==trimws("ND")),NA,as.character(dt1$Station)))
  # dt1$SampleDescription <- as.factor(ifelse((trimws(as.character(dt1$SampleDescription))==trimws("NA")),NA,as.character(dt1$SampleDescription)))
  # dt1$SampleDescription <- as.factor(ifelse((trimws(as.character(dt1$SampleDescription))==trimws("ND")),NA,as.character(dt1$SampleDescription)))
  # dt1$FieldNotes <- as.factor(ifelse((trimws(as.character(dt1$FieldNotes))==trimws("NA")),NA,as.character(dt1$FieldNotes)))
  # dt1$FieldNotes <- as.factor(ifelse((trimws(as.character(dt1$FieldNotes))==trimws("ND")),NA,as.character(dt1$FieldNotes)))
  # dt1$Weather <- as.factor(ifelse((trimws(as.character(dt1$Weather))==trimws("NA")),NA,as.character(dt1$Weather)))
  # dt1$Weather <- as.factor(ifelse((trimws(as.character(dt1$Weather))==trimws("ND")),NA,as.character(dt1$Weather)))
  # dt1$AirTemp <- ifelse((trimws(as.character(dt1$AirTemp))==trimws("NA")),NA,dt1$AirTemp)               
  # suppressWarnings(dt1$AirTemp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$AirTemp))==as.character(as.numeric("NA"))),NA,dt1$AirTemp))
  # dt1$AirTemp <- ifelse((trimws(as.character(dt1$AirTemp))==trimws("ND")),NA,dt1$AirTemp)               
  # suppressWarnings(dt1$AirTemp <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$AirTemp))==as.character(as.numeric("ND"))),NA,dt1$AirTemp))
  # dt1$WindVelocity <- ifelse((trimws(as.character(dt1$WindVelocity))==trimws("NA")),NA,dt1$WindVelocity)               
  # suppressWarnings(dt1$WindVelocity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$WindVelocity))==as.character(as.numeric("NA"))),NA,dt1$WindVelocity))
  # dt1$WindVelocity <- ifelse((trimws(as.character(dt1$WindVelocity))==trimws("ND")),NA,dt1$WindVelocity)               
  # suppressWarnings(dt1$WindVelocity <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$WindVelocity))==as.character(as.numeric("ND"))),NA,dt1$WindVelocity))
  # dt1$WindDirection <- ifelse((trimws(as.character(dt1$WindDirection))==trimws("NA")),NA,dt1$WindDirection)               
  # suppressWarnings(dt1$WindDirection <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$WindDirection))==as.character(as.numeric("NA"))),NA,dt1$WindDirection))
  # dt1$WindDirection <- ifelse((trimws(as.character(dt1$WindDirection))==trimws("ND")),NA,dt1$WindDirection)               
  # suppressWarnings(dt1$WindDirection <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$WindDirection))==as.character(as.numeric("ND"))),NA,dt1$WindDirection))
  # dt1$NorthLat <- ifelse((trimws(as.character(dt1$NorthLat))==trimws("NA")),NA,dt1$NorthLat)               
  # suppressWarnings(dt1$NorthLat <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$NorthLat))==as.character(as.numeric("NA"))),NA,dt1$NorthLat))
  # dt1$NorthLat <- ifelse((trimws(as.character(dt1$NorthLat))==trimws("ND")),NA,dt1$NorthLat)               
  # suppressWarnings(dt1$NorthLat <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$NorthLat))==as.character(as.numeric("ND"))),NA,dt1$NorthLat))
  # dt1$WestLong <- ifelse((trimws(as.character(dt1$WestLong))==trimws("NA")),NA,dt1$WestLong)               
  # suppressWarnings(dt1$WestLong <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$WestLong))==as.character(as.numeric("NA"))),NA,dt1$WestLong))
  # dt1$WestLong <- ifelse((trimws(as.character(dt1$WestLong))==trimws("ND")),NA,dt1$WestLong)               
  # suppressWarnings(dt1$WestLong <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$WestLong))==as.character(as.numeric("ND"))),NA,dt1$WestLong))
  # dt1$Chla <- ifelse((trimws(as.character(dt1$Chla))==trimws("NA")),NA,dt1$Chla)               
  # suppressWarnings(dt1$Chla <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Chla))==as.character(as.numeric("NA"))),NA,dt1$Chla))
  # dt1$Chla <- ifelse((trimws(as.character(dt1$Chla))==trimws("ND")),NA,dt1$Chla)               
  # suppressWarnings(dt1$Chla <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$Chla))==as.character(as.numeric("ND"))),NA,dt1$Chla))
  # dt1$Pheophytin <- ifelse((trimws(as.character(dt1$Pheophytin))==trimws("NA")),NA,dt1$Pheophytin)               
  # suppressWarnings(dt1$Pheophytin <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Pheophytin))==as.character(as.numeric("NA"))),NA,dt1$Pheophytin))
  # dt1$Pheophytin <- ifelse((trimws(as.character(dt1$Pheophytin))==trimws("ND")),NA,dt1$Pheophytin)               
  # suppressWarnings(dt1$Pheophytin <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$Pheophytin))==as.character(as.numeric("ND"))),NA,dt1$Pheophytin))
  # dt1$TotAlkalinity <- ifelse((trimws(as.character(dt1$TotAlkalinity))==trimws("NA")),NA,dt1$TotAlkalinity)               
  # suppressWarnings(dt1$TotAlkalinity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TotAlkalinity))==as.character(as.numeric("NA"))),NA,dt1$TotAlkalinity))
  # dt1$TotAlkalinity <- ifelse((trimws(as.character(dt1$TotAlkalinity))==trimws("ND")),NA,dt1$TotAlkalinity)               
  # suppressWarnings(dt1$TotAlkalinity <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$TotAlkalinity))==as.character(as.numeric("ND"))),NA,dt1$TotAlkalinity))
  # dt1$TotAmmonia <- ifelse((trimws(as.character(dt1$TotAmmonia))==trimws("NA")),NA,dt1$TotAmmonia)               
  # suppressWarnings(dt1$TotAmmonia <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TotAmmonia))==as.character(as.numeric("NA"))),NA,dt1$TotAmmonia))
  # dt1$TotAmmonia <- ifelse((trimws(as.character(dt1$TotAmmonia))==trimws("ND")),NA,dt1$TotAmmonia)               
  # suppressWarnings(dt1$TotAmmonia <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$TotAmmonia))==as.character(as.numeric("ND"))),NA,dt1$TotAmmonia))
  # dt1$DissAmmonia <- ifelse((trimws(as.character(dt1$DissAmmonia))==trimws("NA")),NA,dt1$DissAmmonia)               
  # suppressWarnings(dt1$DissAmmonia <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissAmmonia))==as.character(as.numeric("NA"))),NA,dt1$DissAmmonia))
  # dt1$DissAmmonia <- ifelse((trimws(as.character(dt1$DissAmmonia))==trimws("ND")),NA,dt1$DissAmmonia)               
  # suppressWarnings(dt1$DissAmmonia <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$DissAmmonia))==as.character(as.numeric("ND"))),NA,dt1$DissAmmonia))
  # dt1$DissBromide <- ifelse((trimws(as.character(dt1$DissBromide))==trimws("NA")),NA,dt1$DissBromide)               
  # suppressWarnings(dt1$DissBromide <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissBromide))==as.character(as.numeric("NA"))),NA,dt1$DissBromide))
  # dt1$DissBromide <- ifelse((trimws(as.character(dt1$DissBromide))==trimws("ND")),NA,dt1$DissBromide)               
  # suppressWarnings(dt1$DissBromide <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$DissBromide))==as.character(as.numeric("ND"))),NA,dt1$DissBromide))
  # dt1$DissCalcium <- ifelse((trimws(as.character(dt1$DissCalcium))==trimws("NA")),NA,dt1$DissCalcium)               
  # suppressWarnings(dt1$DissCalcium <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissCalcium))==as.character(as.numeric("NA"))),NA,dt1$DissCalcium))
  # dt1$DissCalcium <- ifelse((trimws(as.character(dt1$DissCalcium))==trimws("ND")),NA,dt1$DissCalcium)               
  # suppressWarnings(dt1$DissCalcium <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$DissCalcium))==as.character(as.numeric("ND"))),NA,dt1$DissCalcium))
  # dt1$TotChloride <- ifelse((trimws(as.character(dt1$TotChloride))==trimws("NA")),NA,dt1$TotChloride)               
  # suppressWarnings(dt1$TotChloride <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TotChloride))==as.character(as.numeric("NA"))),NA,dt1$TotChloride))
  # dt1$TotChloride <- ifelse((trimws(as.character(dt1$TotChloride))==trimws("ND")),NA,dt1$TotChloride)               
  # suppressWarnings(dt1$TotChloride <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$TotChloride))==as.character(as.numeric("ND"))),NA,dt1$TotChloride))
  # dt1$DissChloride <- ifelse((trimws(as.character(dt1$DissChloride))==trimws("NA")),NA,dt1$DissChloride)               
  # suppressWarnings(dt1$DissChloride <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissChloride))==as.character(as.numeric("NA"))),NA,dt1$DissChloride))
  # dt1$DissChloride <- ifelse((trimws(as.character(dt1$DissChloride))==trimws("ND")),NA,dt1$DissChloride)               
  # suppressWarnings(dt1$DissChloride <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$DissChloride))==as.character(as.numeric("ND"))),NA,dt1$DissChloride))
  # dt1$DissNitrateNitrite <- ifelse((trimws(as.character(dt1$DissNitrateNitrite))==trimws("NA")),NA,dt1$DissNitrateNitrite)               
  # suppressWarnings(dt1$DissNitrateNitrite <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissNitrateNitrite))==as.character(as.numeric("NA"))),NA,dt1$DissNitrateNitrite))
  # dt1$DissNitrateNitrite <- ifelse((trimws(as.character(dt1$DissNitrateNitrite))==trimws("ND")),NA,dt1$DissNitrateNitrite)               
  # suppressWarnings(dt1$DissNitrateNitrite <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$DissNitrateNitrite))==as.character(as.numeric("ND"))),NA,dt1$DissNitrateNitrite))
  # dt1$DOC <- ifelse((trimws(as.character(dt1$DOC))==trimws("NA")),NA,dt1$DOC)               
  # suppressWarnings(dt1$DOC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOC))==as.character(as.numeric("NA"))),NA,dt1$DOC))
  # dt1$DOC <- ifelse((trimws(as.character(dt1$DOC))==trimws("ND")),NA,dt1$DOC)               
  # suppressWarnings(dt1$DOC <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$DOC))==as.character(as.numeric("ND"))),NA,dt1$DOC))
  # dt1$TOC <- ifelse((trimws(as.character(dt1$TOC))==trimws("NA")),NA,dt1$TOC)               
  # suppressWarnings(dt1$TOC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TOC))==as.character(as.numeric("NA"))),NA,dt1$TOC))
  # dt1$TOC <- ifelse((trimws(as.character(dt1$TOC))==trimws("ND")),NA,dt1$TOC)               
  # suppressWarnings(dt1$TOC <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$TOC))==as.character(as.numeric("ND"))),NA,dt1$TOC))
  # dt1$DON <- ifelse((trimws(as.character(dt1$DON))==trimws("NA")),NA,dt1$DON)               
  # suppressWarnings(dt1$DON <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DON))==as.character(as.numeric("NA"))),NA,dt1$DON))
  # dt1$DON <- ifelse((trimws(as.character(dt1$DON))==trimws("ND")),NA,dt1$DON)               
  # suppressWarnings(dt1$DON <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$DON))==as.character(as.numeric("ND"))),NA,dt1$DON))
  # dt1$TON <- ifelse((trimws(as.character(dt1$TON))==trimws("NA")),NA,dt1$TON)               
  # suppressWarnings(dt1$TON <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TON))==as.character(as.numeric("NA"))),NA,dt1$TON))
  # dt1$TON <- ifelse((trimws(as.character(dt1$TON))==trimws("ND")),NA,dt1$TON)               
  # suppressWarnings(dt1$TON <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$TON))==as.character(as.numeric("ND"))),NA,dt1$TON))
  # dt1$DissOrthophos <- ifelse((trimws(as.character(dt1$DissOrthophos))==trimws("NA")),NA,dt1$DissOrthophos)               
  # suppressWarnings(dt1$DissOrthophos <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissOrthophos))==as.character(as.numeric("NA"))),NA,dt1$DissOrthophos))
  # dt1$DissOrthophos <- ifelse((trimws(as.character(dt1$DissOrthophos))==trimws("ND")),NA,dt1$DissOrthophos)               
  # suppressWarnings(dt1$DissOrthophos <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$DissOrthophos))==as.character(as.numeric("ND"))),NA,dt1$DissOrthophos))
  # dt1$TotPhos <- ifelse((trimws(as.character(dt1$TotPhos))==trimws("NA")),NA,dt1$TotPhos)               
  # suppressWarnings(dt1$TotPhos <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TotPhos))==as.character(as.numeric("NA"))),NA,dt1$TotPhos))
  # dt1$TotPhos <- ifelse((trimws(as.character(dt1$TotPhos))==trimws("ND")),NA,dt1$TotPhos)               
  # suppressWarnings(dt1$TotPhos <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$TotPhos))==as.character(as.numeric("ND"))),NA,dt1$TotPhos))
  # dt1$DissSilica <- ifelse((trimws(as.character(dt1$DissSilica))==trimws("NA")),NA,dt1$DissSilica)               
  # suppressWarnings(dt1$DissSilica <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissSilica))==as.character(as.numeric("NA"))),NA,dt1$DissSilica))
  # dt1$DissSilica <- ifelse((trimws(as.character(dt1$DissSilica))==trimws("ND")),NA,dt1$DissSilica)               
  # suppressWarnings(dt1$DissSilica <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$DissSilica))==as.character(as.numeric("ND"))),NA,dt1$DissSilica))
  # dt1$TDS <- ifelse((trimws(as.character(dt1$TDS))==trimws("NA")),NA,dt1$TDS)               
  # suppressWarnings(dt1$TDS <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TDS))==as.character(as.numeric("NA"))),NA,dt1$TDS))
  # dt1$TDS <- ifelse((trimws(as.character(dt1$TDS))==trimws("ND")),NA,dt1$TDS)               
  # suppressWarnings(dt1$TDS <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$TDS))==as.character(as.numeric("ND"))),NA,dt1$TDS))
  # dt1$TSS <- ifelse((trimws(as.character(dt1$TSS))==trimws("NA")),NA,dt1$TSS)               
  # suppressWarnings(dt1$TSS <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TSS))==as.character(as.numeric("NA"))),NA,dt1$TSS))
  # dt1$TSS <- ifelse((trimws(as.character(dt1$TSS))==trimws("ND")),NA,dt1$TSS)               
  # suppressWarnings(dt1$TSS <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$TSS))==as.character(as.numeric("ND"))),NA,dt1$TSS))
  # dt1$VSS <- ifelse((trimws(as.character(dt1$VSS))==trimws("NA")),NA,dt1$VSS)               
  # suppressWarnings(dt1$VSS <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$VSS))==as.character(as.numeric("NA"))),NA,dt1$VSS))
  # dt1$VSS <- ifelse((trimws(as.character(dt1$VSS))==trimws("ND")),NA,dt1$VSS)               
  # suppressWarnings(dt1$VSS <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$VSS))==as.character(as.numeric("ND"))),NA,dt1$VSS))
  # dt1$TKN <- ifelse((trimws(as.character(dt1$TKN))==trimws("NA")),NA,dt1$TKN)               
  # suppressWarnings(dt1$TKN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TKN))==as.character(as.numeric("NA"))),NA,dt1$TKN))
  # dt1$TKN <- ifelse((trimws(as.character(dt1$TKN))==trimws("ND")),NA,dt1$TKN)               
  # suppressWarnings(dt1$TKN <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$TKN))==as.character(as.numeric("ND"))),NA,dt1$TKN))
  # dt1$Depth <- ifelse((trimws(as.character(dt1$Depth))==trimws("NA")),NA,dt1$Depth)               
  # suppressWarnings(dt1$Depth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Depth))==as.character(as.numeric("NA"))),NA,dt1$Depth))
  # dt1$Depth <- ifelse((trimws(as.character(dt1$Depth))==trimws("ND")),NA,dt1$Depth)               
  # suppressWarnings(dt1$Depth <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$Depth))==as.character(as.numeric("ND"))),NA,dt1$Depth))
  # dt1$Secchi <- ifelse((trimws(as.character(dt1$Secchi))==trimws("NA")),NA,dt1$Secchi)               
  # suppressWarnings(dt1$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Secchi))==as.character(as.numeric("NA"))),NA,dt1$Secchi))
  # dt1$Secchi <- ifelse((trimws(as.character(dt1$Secchi))==trimws("ND")),NA,dt1$Secchi)               
  # suppressWarnings(dt1$Secchi <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$Secchi))==as.character(as.numeric("ND"))),NA,dt1$Secchi))
  # dt1$Microcystis <- ifelse((trimws(as.character(dt1$Microcystis))==trimws("NA")),NA,dt1$Microcystis)               
  # suppressWarnings(dt1$Microcystis <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Microcystis))==as.character(as.numeric("NA"))),NA,dt1$Microcystis))
  # dt1$Microcystis <- ifelse((trimws(as.character(dt1$Microcystis))==trimws("ND")),NA,dt1$Microcystis)               
  # suppressWarnings(dt1$Microcystis <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$Microcystis))==as.character(as.numeric("ND"))),NA,dt1$Microcystis))
  # dt1$LightExtinction <- ifelse((trimws(as.character(dt1$LightExtinction))==trimws("NA")),NA,dt1$LightExtinction)               
  # suppressWarnings(dt1$LightExtinction <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$LightExtinction))==as.character(as.numeric("NA"))),NA,dt1$LightExtinction))
  # dt1$LightExtinction <- ifelse((trimws(as.character(dt1$LightExtinction))==trimws("ND")),NA,dt1$LightExtinction)               
  # suppressWarnings(dt1$LightExtinction <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$LightExtinction))==as.character(as.numeric("ND"))),NA,dt1$LightExtinction))
  # dt1$SpCndSurface <- ifelse((trimws(as.character(dt1$SpCndSurface))==trimws("NA")),NA,dt1$SpCndSurface)               
  # suppressWarnings(dt1$SpCndSurface <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SpCndSurface))==as.character(as.numeric("NA"))),NA,dt1$SpCndSurface))
  # dt1$SpCndSurface <- ifelse((trimws(as.character(dt1$SpCndSurface))==trimws("ND")),NA,dt1$SpCndSurface)               
  # suppressWarnings(dt1$SpCndSurface <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$SpCndSurface))==as.character(as.numeric("ND"))),NA,dt1$SpCndSurface))
  # dt1$DOSurface <- ifelse((trimws(as.character(dt1$DOSurface))==trimws("NA")),NA,dt1$DOSurface)               
  # suppressWarnings(dt1$DOSurface <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOSurface))==as.character(as.numeric("NA"))),NA,dt1$DOSurface))
  # dt1$DOSurface <- ifelse((trimws(as.character(dt1$DOSurface))==trimws("ND")),NA,dt1$DOSurface)               
  # suppressWarnings(dt1$DOSurface <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$DOSurface))==as.character(as.numeric("ND"))),NA,dt1$DOSurface))
  # dt1$DOpercentSurface <- ifelse((trimws(as.character(dt1$DOpercentSurface))==trimws("NA")),NA,dt1$DOpercentSurface)               
  # suppressWarnings(dt1$DOpercentSurface <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOpercentSurface))==as.character(as.numeric("NA"))),NA,dt1$DOpercentSurface))
  # dt1$DOpercentSurface <- ifelse((trimws(as.character(dt1$DOpercentSurface))==trimws("ND")),NA,dt1$DOpercentSurface)               
  # suppressWarnings(dt1$DOpercentSurface <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$DOpercentSurface))==as.character(as.numeric("ND"))),NA,dt1$DOpercentSurface))
  # dt1$WTSurface <- ifelse((trimws(as.character(dt1$WTSurface))==trimws("NA")),NA,dt1$WTSurface)               
  # suppressWarnings(dt1$WTSurface <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$WTSurface))==as.character(as.numeric("NA"))),NA,dt1$WTSurface))
  # dt1$WTSurface <- ifelse((trimws(as.character(dt1$WTSurface))==trimws("ND")),NA,dt1$WTSurface)               
  # suppressWarnings(dt1$WTSurface <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$WTSurface))==as.character(as.numeric("ND"))),NA,dt1$WTSurface))
  # dt1$TurbiditySurface <- as.factor(ifelse((trimws(as.character(dt1$TurbiditySurface))==trimws("NA")),NA,as.character(dt1$TurbiditySurface)))
  # dt1$TurbiditySurface <- as.factor(ifelse((trimws(as.character(dt1$TurbiditySurface))==trimws("ND")),NA,as.character(dt1$TurbiditySurface)))
  # dt1$pHSurface <- ifelse((trimws(as.character(dt1$pHSurface))==trimws("NA")),NA,dt1$pHSurface)               
  # suppressWarnings(dt1$pHSurface <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$pHSurface))==as.character(as.numeric("NA"))),NA,dt1$pHSurface))
  # dt1$pHSurface <- ifelse((trimws(as.character(dt1$pHSurface))==trimws("ND")),NA,dt1$pHSurface)               
  # suppressWarnings(dt1$pHSurface <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$pHSurface))==as.character(as.numeric("ND"))),NA,dt1$pHSurface))
  # dt1$SpCndBottom <- ifelse((trimws(as.character(dt1$SpCndBottom))==trimws("NA")),NA,dt1$SpCndBottom)               
  # suppressWarnings(dt1$SpCndBottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SpCndBottom))==as.character(as.numeric("NA"))),NA,dt1$SpCndBottom))
  # dt1$SpCndBottom <- ifelse((trimws(as.character(dt1$SpCndBottom))==trimws("ND")),NA,dt1$SpCndBottom)               
  # suppressWarnings(dt1$SpCndBottom <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$SpCndBottom))==as.character(as.numeric("ND"))),NA,dt1$SpCndBottom))
  # dt1$DOBottom <- ifelse((trimws(as.character(dt1$DOBottom))==trimws("NA")),NA,dt1$DOBottom)               
  # suppressWarnings(dt1$DOBottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOBottom))==as.character(as.numeric("NA"))),NA,dt1$DOBottom))
  # dt1$DOBottom <- ifelse((trimws(as.character(dt1$DOBottom))==trimws("ND")),NA,dt1$DOBottom)               
  # suppressWarnings(dt1$DOBottom <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$DOBottom))==as.character(as.numeric("ND"))),NA,dt1$DOBottom))
  # dt1$DOpercentBottom <- ifelse((trimws(as.character(dt1$DOpercentBottom))==trimws("NA")),NA,dt1$DOpercentBottom)               
  # suppressWarnings(dt1$DOpercentBottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOpercentBottom))==as.character(as.numeric("NA"))),NA,dt1$DOpercentBottom))
  # dt1$DOpercentBottom <- ifelse((trimws(as.character(dt1$DOpercentBottom))==trimws("ND")),NA,dt1$DOpercentBottom)               
  # suppressWarnings(dt1$DOpercentBottom <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$DOpercentBottom))==as.character(as.numeric("ND"))),NA,dt1$DOpercentBottom))
  # dt1$WTBottom <- ifelse((trimws(as.character(dt1$WTBottom))==trimws("NA")),NA,dt1$WTBottom)               
  # suppressWarnings(dt1$WTBottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$WTBottom))==as.character(as.numeric("NA"))),NA,dt1$WTBottom))
  # dt1$WTBottom <- ifelse((trimws(as.character(dt1$WTBottom))==trimws("ND")),NA,dt1$WTBottom)               
  # suppressWarnings(dt1$WTBottom <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$WTBottom))==as.character(as.numeric("ND"))),NA,dt1$WTBottom))
  # dt1$TurbidityBottom <- ifelse((trimws(as.character(dt1$TurbidityBottom))==trimws("NA")),NA,dt1$TurbidityBottom)               
  # suppressWarnings(dt1$TurbidityBottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TurbidityBottom))==as.character(as.numeric("NA"))),NA,dt1$TurbidityBottom))
  # dt1$TurbidityBottom <- ifelse((trimws(as.character(dt1$TurbidityBottom))==trimws("ND")),NA,dt1$TurbidityBottom)               
  # suppressWarnings(dt1$TurbidityBottom <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$TurbidityBottom))==as.character(as.numeric("ND"))),NA,dt1$TurbidityBottom))
  # dt1$pHBottom <- ifelse((trimws(as.character(dt1$pHBottom))==trimws("NA")),NA,dt1$pHBottom)               
  # suppressWarnings(dt1$pHBottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$pHBottom))==as.character(as.numeric("NA"))),NA,dt1$pHBottom))
  # dt1$pHBottom <- ifelse((trimws(as.character(dt1$pHBottom))==trimws("ND")),NA,dt1$pHBottom)               
  # suppressWarnings(dt1$pHBottom <- ifelse(!is.na(as.numeric("ND")) & (trimws(as.character(dt1$pHBottom))==as.character(as.numeric("ND"))),NA,dt1$pHBottom))
  # 
  # 
  # Here is the structure of the input data frame:
  # str(dt1)                            
  # attach(dt1)                            
  # # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
  # 
  # summary(Station)
  # summary(Date)
  # summary(Time)
  # summary(SampleDescription)
  # summary(FieldNotes)
  # summary(Weather)
  # summary(AirTemp)
  # summary(WindVelocity)
  # summary(WindDirection)
  # summary(NorthLat)
  # summary(WestLong)
  # summary(Chla)
  # summary(Pheophytin)
  # summary(TotAlkalinity)
  # summary(TotAmmonia)
  # summary(DissAmmonia)
  # summary(DissBromide)
  # summary(DissCalcium)
  # summary(TotChloride)
  # summary(DissChloride)
  # summary(DissNitrateNitrite)
  # summary(DOC)
  # summary(TOC)
  # summary(DON)
  # summary(TON)
  # summary(DissOrthophos)
  # summary(TotPhos)
  # summary(DissSilica)
  # summary(TDS)
  # summary(TSS)
  # summary(VSS)
  # summary(TKN)
  # summary(Depth)
  # summary(Secchi)
  # summary(Microcystis)
  # summary(LightExtinction)
  # summary(SpCndSurface)
  # summary(DOSurface)
  # summary(DOpercentSurface)
  # summary(WTSurface)
  # summary(TurbiditySurface)
  # summary(pHSurface)
  # summary(SpCndBottom)
  # summary(DOBottom)
  # summary(DOpercentBottom)
  # summary(WTBottom)
  # summary(TurbidityBottom)
  # summary(pHBottom) 
  #                 # Get more details on character variables
  #                  
  # summary(as.factor(dt1$Station)) 
  # summary(as.factor(dt1$SampleDescription)) 
  # summary(as.factor(dt1$FieldNotes)) 
  # summary(as.factor(dt1$Weather)) 
  # summary(as.factor(dt1$TurbiditySurface))
  # detach(dt1)               
  
  
  inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/458/3/5b439de4be6d3a5964867b0cb4a1f059" 
  infile2 <- tempfile()
  try(download.file(inUrl2,infile2,method="curl"))
  if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")
  
  
  dt2 <-read.csv(infile2,header=F 
                 ,skip=1
                 ,sep=","  
                 ,quot='"' 
                 , col.names=c(
                   "Station",     
                   "StationID",     
                   "Location",     
                   "StationType",     
                   "Latitude",     
                   "Longitude",     
                   "Status"    ), check.names=TRUE)
  
  unlink(infile2)
  
  # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
  
  if (class(dt2$Station)!="factor") dt2$Station<- as.factor(dt2$Station)
  if (class(dt2$StationID)!="factor") dt2$StationID<- as.factor(dt2$StationID)
  if (class(dt2$Location)!="factor") dt2$Location<- as.factor(dt2$Location)
  if (class(dt2$StationType)!="factor") dt2$StationType<- as.factor(dt2$StationType)
  if (class(dt2$Latitude)=="factor") dt2$Latitude <-as.numeric(levels(dt2$Latitude))[as.integer(dt2$Latitude) ]               
  if (class(dt2$Latitude)=="character") dt2$Latitude <-as.numeric(dt2$Latitude)
  if (class(dt2$Longitude)=="factor") dt2$Longitude <-as.numeric(levels(dt2$Longitude))[as.integer(dt2$Longitude) ]               
  if (class(dt2$Longitude)=="character") dt2$Longitude <-as.numeric(dt2$Longitude)
  if (class(dt2$Status)!="factor") dt2$Status<- as.factor(dt2$Status)
  
  # Convert Missing Values to NA for non-dates
  
  dt2$Station <- as.factor(ifelse((trimws(as.character(dt2$Station))==trimws("NA")),NA,as.character(dt2$Station)))
  dt2$StationID <- as.factor(ifelse((trimws(as.character(dt2$StationID))==trimws("NA")),NA,as.character(dt2$StationID)))
  dt2$Location <- as.factor(ifelse((trimws(as.character(dt2$Location))==trimws("NA")),NA,as.character(dt2$Location)))
  dt2$StationType <- as.factor(ifelse((trimws(as.character(dt2$StationType))==trimws("NA")),NA,as.character(dt2$StationType)))
  dt2$Latitude <- ifelse((trimws(as.character(dt2$Latitude))==trimws("NA")),NA,dt2$Latitude)               
  suppressWarnings(dt2$Latitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Latitude))==as.character(as.numeric("NA"))),NA,dt2$Latitude))
  dt2$Longitude <- ifelse((trimws(as.character(dt2$Longitude))==trimws("NA")),NA,dt2$Longitude)               
  suppressWarnings(dt2$Longitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Longitude))==as.character(as.numeric("NA"))),NA,dt2$Longitude))
  dt2$Status <- as.factor(ifelse((trimws(as.character(dt2$Status))==trimws("NA")),NA,as.character(dt2$Status)))
  
  
  # Here is the structure of the input data frame:
  # str(dt2)                            
  # attach(dt2)                            
  # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
  # 
  # summary(Station)
  # summary(StationID)
  # summary(Location)
  # summary(StationType)
  # summary(Latitude)
  # summary(Longitude)
  # summary(Status) 
  #                 # Get more details on character variables
  #                  
  # summary(as.factor(dt2$Station)) 
  # summary(as.factor(dt2$StationID)) 
  # summary(as.factor(dt2$Location)) 
  # summary(as.factor(dt2$StationType)) 
  # summary(as.factor(dt2$Status))
  # detach(dt2)
  
  return(dt1)
}