# Reviewing EMP Phytoplankton Data before EDI Publication --------
# 12/23/2022 

library(tidyverse)
library(lubridate)
library(janitor)

# Set working directory
setwd("./03_Phyto/EMP-phyto-EDI")
getwd()

# Clean workspace
rm(list=ls())

# Importing Processed EMP Data -------------------------------------------------
df_phyto <- read_csv("data/Phyto Data 2008-present_121522.csv")

unique(df_phyto$Lab)

unique(df_phyto$Genus)

sort(unique(df_phyto$TIN))
