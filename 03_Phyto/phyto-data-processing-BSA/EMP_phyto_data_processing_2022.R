# Extracting phyto data from EMP datasheets from BSA for 2022
# 1/10/2023

library("tidyverse");packageVersion("tidyverse")
library("lubridate");packageVersion("lubridate")
library("janitor");packageVersion("janitor")

# Add in data from 2022
phyto_files_2022 <- dir(path = "data/EMP/2022", pattern = "\\.csv", full.names = T)

df_phyto_2022 <- map_dfr(phyto_files_2022, ~read_csv(.x))

# Remove empty rows
df_phyto_2022 <- df_phyto_2022 %>% filter_all(any_vars(!is.na(.)))



