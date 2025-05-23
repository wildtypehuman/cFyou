library(ggplot2)
library(scales)
library(tidyr)
library(stringr)

df_AM_Count <- read.csv("/Users/wildtype.human/bioinformatics/cFu/AM10925_Counts.csv")

df_AM_Count <- df_AM_Count %>%
  pivot_longer(
    cols = -Sample,
    names_to = "Dilution",
    values_to = "Count"
  )
# Rania plated N1 and N2, 50 uL per well, and 3 mL total of each organ lysate
cFu_AM_20250109_Count <- df_AM_Count %>%
  mutate(CFU_Count = case_when(
    str_starts(Dilution, "N1") ~ Count * 10 * 20,
    str_starts(Dilution, "N2") ~ Count * 100 * 20,
    TRUE ~ NA_real_
  ))

# add metadata

cFu_AM_20250109_Count <- cFu_AM_20250109_Count %>%
  mutate(
    Date_Collected = "20250109",
    Collected_By = "AM",
    Experiment_ID = "AM_121324",
    Date_Plated = "Thursday,_January_9,_2025",
    Date_Read = "Monday,_January_27,_2025",
    MTB_Strain_Type = "H37Rv",
    Strain_Modifications = "pMV261-ZsGreen",
    Infection_Route = "aerosol",
    Mouse_Type = "B6_and_6T",
    Treatment = "none",
    Organ_Tissue = "Lung",
    Sample_Storage = "plated_fresh",
    Preprocessing = "single_cell_suspension_diluted_in_PBS-Tween",
    Tissue_Homogenization_Method = "GentleMACS",
    Timepoint_dpi = 27,
    Log_Dilution_Factor = "10^-2",
    Media = "HBSS_+_PBS",
    Media_Additive = "FBS,_EDTA,_Liberase,_DNase",
    Plate_Type = "12_well",
    Spreading_Method = "rotation",
    Total_Plates = 5,
    Number_of_Replicates = 3,
    Volume_Per_Plate_uL = 50
  )

# combined_df <- left_join(df_AM_Count, dat_AM_10925, by = "Sample")
# 
# combined_df <- combined_df %>%
#   select(-Well, -Omit, -Target, -Dye, -Task)
# 
# cFu_AM_20250109_TaqAndCount <- combined_df %>%
#   mutate(Date_Collected = "20250109", Collected_By = "AM")
# 
# # Export as CSV
# write.csv(
#   cFu_AM_20250109_TaqAndCount,
#   file = "cFu_AM_20250109_TaqAndCount.csv",
#   row.names = FALSE
# )
