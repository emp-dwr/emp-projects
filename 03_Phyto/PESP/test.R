
# SETUP -------------------------------------------------------------------

## set wd
setwd('./03_Phyto/PESP/')

## import packages
library(tidyverse)
'%!in%' <- function(x,y)!('%in%'(x,y))

## read in data
df_syn <- read_csv('data/EMP/Phyto Classification.csv', show_col_types = FALSE)
df_data <- read_csv('data/EMP/Phyto Data 2008-present_121522.csv', show_col_types = FALSE)

## remove unnecessary columns
df_data <- df_data %>%
  select(-c(Taxonomist, LabNumber, TIN))

## remove percents from percent col
df_data$Percent_Sample_Counted <- gsub('%','',df_data$Percent_Sample_Counted)


# FIX UNKNOWN -------------------------------------------------------------
unknown_syns <- 'unknown|unidentified|Unidentified|Undetermined|undetermined'

df_data <- df_data %>%
  # update Taxon column to standardize Unknown
  mutate(
    Taxon = case_when(grepl(unknown_syns,Taxon) ~ str_replace_all(Taxon, unknown_syns, 'Unknown'),
                      TRUE ~ Taxon)
  ) %>%
  # Update Genus column if unknown Species
  mutate(
    Genus = case_when(grepl('Unknown', Taxon) ~ 'Unknown',
                      is.na(Genus) ~ 'Unknown',
                      Genus == 'Other' ~ 'Unknown',
                      Genus == 'genus' ~ 'Unknown',
                      TRUE ~ Genus)
  ) %>%
  # Update Species column in unknown
  mutate(
    Species = case_when(Genus == 'Unknown' ~ 'Unknown',
                        is.na(Species) ~ 'Unknown',
                        TRUE ~ Species)
  )

# JOIN SYN/TAXA COLS ------------------------------------------------------


