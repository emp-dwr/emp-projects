# Load required packages
library(tidyverse)
library(lubridate)
library(here)

# 1. Load the dataset
# Replace with your actual file path if needed
# Load data from EMP EDI (through 2024) ----------------------------------------
df <- read_csv(file = here("03_Phyto",
                           "data-format-conversion-for-PRIMER",
                           "emp-data-primer-upload.csv"),
               show_col_types = FALSE)

# Identify which taxa have more than one unique set of metadata
inconsistent_taxa <- df %>%
  select(Taxon, `Algal Type`, Genus) %>%
  mutate(Taxon = str_squish(Taxon)) %>% # Strip whitespace just in case
  distinct() %>%
  count(Taxon) %>%
  filter(n > 1)

# View the actual conflicting rows side-by-side
conflicting_metadata <- df %>%
  mutate(Taxon = str_squish(Taxon)) %>%
  select(Taxon, `Algal Type`, Genus) %>%
  filter(Taxon %in% inconsistent_taxa$Taxon) %>%
  distinct() %>%
  arrange(Taxon)

print(conflicting_metadata)

# 2. Clean specific taxonomy errors --------------------------------------------
df_cleaned <- df %>%
  mutate(
    Taxon = str_squish(Taxon), 
    `Algal Type` = case_when(
      Taxon == "Paralia sulcata" ~ "Centric Diatom",
      Taxon == "cf. Paralia sp." ~ "Centric Diatom",
      Taxon == "Pleurosira sp." ~ "Centric Diatom",
      Taxon == "Chrysochromulina sp." ~ "Haptophyte",
      TRUE ~ `Algal Type` 
    )
  )

# 3. Process, aggregate, and format for PRIMER ---------------------------------
primer_single_file <- df_cleaned %>%
  mutate(
    ParsedDate = mdy(SampleDate),
    Month = month(ParsedDate, label = TRUE, abbr = TRUE),
    Year = year(ParsedDate),
    SampleID = paste0(Month, ".", Year, "_", StationCode)
  ) %>%
  group_by(
    SampleID, Taxon, 
    SampleDate, Month, Year, StationCode, 
    `Algal Type`, Genus
  ) %>%
  summarise(`Organisms per mL` = sum(`Organisms per mL`,na.rm = TRUE), 
            .groups = "drop") %>%
  # Create the blank columns
  mutate(
    Blank_Factors = NA_character_,
    Blank_Labels = NA_character_
  ) %>%
  # Order them perfectly
  select(
    SampleID, Taxon, `Organisms per mL`, 
    Blank_Factors, SampleDate, Month, Year, StationCode,
    Blank_Labels, `Algal Type`, Genus
  )

# 4. Final Formatting and Data Exporting

# Create a custom header string with truly empty spots for the delimiters ---
# Notice the consecutive commas (e.g., "mL,,SampleDate" and "StationCode,,Algal Type") 
custom_header <- "SampleID,Taxon,Organisms per mL,,SampleDate,Month,Year,StationCode,,Algal Type,Genus,Species"

# Write the file in two steps to guarantee perfect formatting

# Step A: Write our custom header string to create the file
write_lines(custom_header, "primer7_single_upload.csv")

# Step B: Append the actual dataframe underneath, telling R to leave its headers behind
write_csv(primer_single_file, 
          here("03_Phyto",
               "data-format-conversion-for-PRIMER",
               "primer7_single_upload.csv"),
          append = TRUE, 
          col_names = FALSE, 
          na = "")
