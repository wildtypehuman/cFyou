library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(stringr)

df_AM_Count <- read.csv("/Users/wildtype.human/bioinformatics/cFu/AM_112224_Count.csv")


df_AM_Count <- df_AM_Count %>%
  pivot_longer(
    cols = -Sample,
    names_to = "Dilution",
    values_to = "Count"
  )

cFu_AM_20241219_Count <- df_AM_Count %>%
  mutate(CFU_Count = case_when(
    str_starts(Dilution, "N1") ~ Count * 10 * 20,
    str_starts(Dilution, "N2") ~ Count * 100 * 20,
    str_starts(Dilution, "N3") ~ Count * 1000 * 20,
    TRUE ~ NA_real_
  ))

#Add metadata

cFu_AM_20241219_Count <- cFu_AM_20241219_Count %>%
  mutate(
    Date_Collected = "20241219",
    Collected_By = "AM",
    Experiment_ID = "AM_112224",
    Date_Plated = "Thursday,_December_19,_2024",
    Date_Read = "Thursday,_January_9,_2025",
    MTB_Strain_Type = "H37Rv",
    Strain_Modifications = "pMV261-ZsGreen",
    Infection_Route = "aerosol",
    Mouse_Type = "SP140",
    Treatment = "none",
    Organ_Tissue = "sorted_lung_phagocytes",
    Sample_Storage = "plated_fresh",
    Preprocessing = "sorted_cells_resuspended_in_PBS-Tween",
    Tissue_Homogenization_Method = "GentleMACS_then_flow_sorting",
    Timepoint_dpi = 27,
    Log_Dilution_Factor = "10^-2",
    Media = "n/a",
    Media_Additive = "n/a",
    Plate_Type = "12_well",
    Spreading_Method = "rotation",
    Total_Plates = 16,
    Number_of_Replicates = 3,
    Volume_Per_Plate_uL = 50
  )

# Export final CFU data

write.csv(cFu_AM_20241219_Count, file = "cFu_AM_20241219_Count.csv", row.names = FALSE)


# combined_df <- left_join(dat_AM_21425, df_AM_Count, by = "Sample")
# 
# combined_df <- combined_df %>%
#   select(-Well, -Omit, -Target, -Dye, -Task)
# 
# cFu_AM_20241219_TaqAndCount <- combined_df %>%
#   mutate(Date_Collected = "20241219", Collected_By = "AM")
# 
# # Export as CSV
# write.csv(
#   cFu_AM_20241219_TaqAndCount,
#   file = "cFu_AM_20241219_TaqAndCount.csv",
#   row.names = FALSE
# )

