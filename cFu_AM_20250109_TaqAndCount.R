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
df_AM_Count <- df_AM_Count %>%
  mutate(CFU_Count = case_when(
    str_starts(Dilution, "N1") ~ Count * 10 * 20,
    str_starts(Dilution, "N2") ~ Count * 100 * 20,
    TRUE ~ NA_real_
  ))

combined_df <- left_join(df_AM_Count, dat_AM_10925, by = "Sample")

combined_df <- combined_df %>%
  select(-Well, -Omit, -Target, -Dye, -Task)

cFu_AM_20250109_TaqAndCount <- combined_df %>%
  mutate(Date_Collected = "20250109", Collected_By = "AM")

# Export as CSV
write.csv(
  cFu_AM_20250109_TaqAndCount,
  file = "cFu_AM_20250109_TaqAndCount.csv",
  row.names = FALSE
)
