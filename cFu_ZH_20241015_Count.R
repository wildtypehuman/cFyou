library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(stringr)

dat_ZH_Count <- read.csv("/Users/wildtype.human/bioinformatics/cFu/cFu_ZH_20241015Counts.csv")

#Pivot Longer
dat_ZH_Count <- dat_ZH_Count %>%
  pivot_longer(
    cols = -Sample,
    names_to = "Dilution",
    values_to = "Count"
  )

#Zach plates 50 µL per 12 wellplate, so transform per mL


cFu_ZH_20241015_Count <- dat_ZH_Count %>%
  mutate(
    CFU_Count = case_when(
      str_starts(Dilution, "N3") ~ Count * 1000 * 20,
      str_starts(Dilution, "N4") ~ Count * 10000 * 20,
      str_starts(Dilution, "N5") ~ Count * 100000 * 20,
      TRUE ~ NA_real_
    )
  )



#add metadata


cFu_ZH_20241015_Count <- cFu_ZH_20241015_Count %>%
  mutate(
    Date_Collected = "20241015",
    Collected_By = "ZH",
    Experiment_ID = "Adoptive_transfer_sort",
    Date_Plated = "Wednesday,_October_16,_2024",
    Date_Read = "Tuesday,_November_12,_2024",
    MTB_Strain_Type = "Erdman",
    Strain_Modifications = "none",
    Infection_Route = "aerosol",
    Mouse_Type = "SP140-/-",
    Treatment = "none",
    Organ_Tissue = "Lung",
    Sample_Storage = "4C_overnight",
    Preprocessing = "Digested_with_LibTM_DNase",
    Tissue_Homogenization_Method = "GentleMACS",
    Timepoint_dpi = 25,
    Log_Dilution_Factor = NA,  # missing value in your example
    Media = "7H11",
    Media_Additive = "none",
    Plate_Type = "24_well",
    Spreading_Method = "none",
    Total_Plates = 11,
    Number_of_Replicates = 3,
    Volume_Per_Plate_uL = 20
  )

# Export as CSV
write.csv(
  cFu_ZH_20241015_Count,
  file = "cFu_ZH_20241015_Count.csv",
  row.names = FALSE
)

