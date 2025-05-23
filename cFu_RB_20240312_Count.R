#Analyze Rania's Counts from Dec 2024, pepare a df to merge into master

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(stringr)

df_RB_Count <- read.csv("/Users/wildtype.human/bioinformatics/cFu/cFu_RB20241203.csv")

df_RB_Count <- df_RB_Count %>%
  pivot_longer(
    cols = -Sample,
    names_to = "Dilution",
    values_to = "Count"
  )
# Rania plated N1 and N2, 50 uL per well, and 3 mL total of each organ lysate
cFu_RB_20240312_Count <- df_RB_Count %>%
  mutate(CFU_Count = case_when(
    str_starts(Dilution, "N1") ~ Count * 10 * 20,
    str_starts(Dilution, "N2") ~ Count * 100 * 20,
    TRUE ~ NA_real_
  ))


# Add metadata 
cFu_RB_20240312_Count <- cFu_RB_20240312_Count %>%
  mutate(
    Date_Collected = "20240312",
    Collected_By = "RB",
    Experiment_ID = "mabs-WT-NLRP3KO1",
    Date_Plated = "Friday,_November_15,_2024",
    Date_Read = "Tuesday,_December_3,_2024",
    MTB_Strain_Type = "HN878",
    Strain_Modifications = "NA",
    Infection_Route = "Aerosol",
    Mouse_Type = "C57BL6",
    Treatment = "monoclonal_antibodies",
    Organ_Tissue = "Lung",
    Sample_Storage = "yes",
    Preprocessing = "Enzymatic_digestion",
    Tissue_Homogenization_Method = "GentleMACS",
    Timepoint_dpi = NA,  
    Log_Dilution_Factor = "0,_1,_2",
    Media = "7H11",
    Media_Additive = "PANTA",
    Plate_Type = "100_mm_petri_dishes",
    Spreading_Method = "none",
    Total_Plates = 56,
    Number_of_Replicates = 2,
    Volume_Per_Plate_uL = 150
  )

# Export as a csv. 
write.csv(cFu_RB_20240312_Count, file = "cFu_RB_20240312_Count.csv", row.names = FALSE)

# #Merge and omit redundant wells
# 
# combined_df <- left_join(datR, df_RB_Count, by = "Sample")
# 
# combined_df <- combined_df %>%
#   select(-Well, -Omit, -Target, -Dye, -Task)
# 
# cFu_RB_20240312_TaqAndCount <- combined_df %>%
#   mutate(Date_Collected = "20240312", Collected_By = "RB")
# 
# # Export as CSV
# write.csv(
#   cFu_RB_20240312_TaqAndCount,
#   file = "cFu_RB_20240312_TaqAndCount.csv",
#   row.names = FALSE
# )




