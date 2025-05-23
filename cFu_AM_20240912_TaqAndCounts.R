library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(stringr)

dat_AM_Count <- read.csv("/Users/wildtype.human/bioinformatics/cFu/AM_081524_Counts.csv")

#Pivot Longer
dat_AM_Count <- dat_AM_Count %>%
  pivot_longer(
    cols = -Sample,
    names_to = "Dilution",
    values_to = "Count"
  )

#Alex plated 50 microliters of 1 to 200 dilution of the spleen and 50 microliters of 1 to 20,000 dilution of the lung.

cFu_AM_20240912_Count <- dat_AM_Count %>%
  mutate(
    CFU_Count = case_when(
      str_starts(Dilution, "N_2") ~ Count * 200 * 20,
      str_starts(Dilution, "N_4") ~ Count * 20000 * 20,
      TRUE ~ NA_real_
    )
  )

# Add metadata

cFu_AM_20240912_Count <- cFu_AM_20240912_Count %>%
  mutate(
    Date_Collected = "20240912",
    Collected_By = "AM",
    Experiment_ID = "AM_081524",
    Date_Plated = "Thursday,_September_12,_2024",
    Date_Read = "Tuesday,_October_8,_2024",
    MTB_Strain_Type = "H37Rv",
    Strain_Modifications = "pMV261-YFP",
    Infection_Route = "aerosol",
    Mouse_Type = "B6,_Ccr2_KO,_Ccr2-Cx3cr1_DKO",
    Treatment = "none",
    Organ_Tissue = "lung_and_lymph_node",
    Sample_Storage = "plated_fresh",
    Preprocessing = "single_cell_suspension_diluted_in_PBS-Tween",
    Tissue_Homogenization_Method = "GentleMACS",
    Timepoint_dpi = 28,
    Log_Dilution_Factor = "10^-1_to_10^-4",
    Media = "HBSS_+_PBS",
    Media_Additive = "FBS,_EDTA,_Liberase,_DNase",
    Plate_Type = "12_well",
    Spreading_Method = "rotation",
    Total_Plates = 24,
    Number_of_Replicates = 3,
    Volume_Per_Plate_uL = 50
  )


# 
# combined_df <- left_join(dat_AM_Count, dat_AM_91224, by = "Sample")
# 
# combined_df <- combined_df %>%
#   select(-Well, -Omit, -Target, -Dye, -Task)
# 
# 
# cFu_AM_20240912_TaqAndCounts <- combined_df %>%
#   mutate(Date_Collected = "20240912", Collected_By = "AM")
# 
# # Export as CSV
# write.csv(
#   cFu_AM_20240912_TaqAndCounts,
#   file = "cFu_AM_20240912_TaqAndCounts.csv",
#   row.names = FALSE
# )






