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
df_RB_Count <- df_RB_Count %>%
  mutate(CFU_Count = case_when(
    str_starts(Dilution, "N1") ~ Count * 10 * 20,
    str_starts(Dilution, "N2") ~ Count * 100 * 20,
    TRUE ~ NA_real_
  ))




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




