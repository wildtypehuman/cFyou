require(ggplot2)
require(tidyverse)

#Load and pivot data

datzw135C <- read.csv("/Users/wildtype.human/bioinformatics/cFu/cFu_WZ_20250513_RawCount.csv")

datzw135C <- datzw135C %>%
  pivot_longer(
    cols = -Sample,
    names_to = "Dilution",
    values_to = "Count"
  )

#Calculate CFU Counts, rename

cFu_ZW_20250513_Count <- datzw135C %>%
  mutate(
    CFU_Count = case_when(
      str_starts(Dilution, "N0") ~ Count * 1 * 50,
      str_starts(Dilution, "N1") ~ Count * 10 * 50,
      str_starts(Dilution, "N2") ~ Count * 100 * 50,
      TRUE ~ NA_real_
    )
  )


#Add metadata

cFu_ZW_20250513_Count <- cFu_ZW_20250513_Count %>%
  mutate(
    Experiment_ID = "zw135",
    Collected_By = "WZ",
    Alina_Dilution = "None",
    Date_Plated = "Tuesday, May 13, 2025",
    Date_Read = "Monday, June 2, 2025",
    MTB_Strain_Type = "Rv/pMv261-zsGreen",
    Strain_Modifications = NA,
    Infection_Route = "Aerosol",
    Mouse_Type = "B6, Ifnar1-/-",
    Treatment = NA,
    Organ_Tissue = "Spleen",
    Sample_Storage = "Frozen after CFU",
    Preprocessing = "Mash in 6mL PBST",
    Tissue_Homogenization_Method = "Mash in 6mL PBST",
    Timepoint_dpi = 28,
    Log_Dilution_Factor = "-1, -2, -3, -4",
    Media = "7H11",
    Media_Additive = "none",
    Plate_Type = "12 well",
    Spreading_Method = NA,
    Total_Plates = NA,
    Number_of_Replicates = 3,
    Volume_Per_Plate_uL = 50
  )



write.csv(cFu_ZW_20250513_Count, file = "cFu_ZW_20250513_Count.csv", row.names = FALSE)



