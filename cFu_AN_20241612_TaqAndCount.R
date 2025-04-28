library(ggplot2)
library(tidyr)
library(dplyr)


datC <- read.csv("/Users/wildtype.human/bioinformatics/cFu/Army_Prelim_CFU_Counts.csv")

datS3_Count <- datC %>%
  filter(Count > 1, Time_Point == "Day 3", Organ == "Spleen")

datS3_Count <- datS3_Count %>%
  mutate(
    Count = as.numeric(Count), 
    CFU_per_mL = Count * 50 * case_when(
      Dilution == "N2" ~ 100,
      Dilution == "N3" ~ 1000,
      Dilution == "N4" ~ 10000,
      TRUE ~ 1 # Default multiplier is 1 if Dilution is not "N6" or "N7"
    )
  )

# Change dat to add Sample column, so I can merge taqman and count
write.csv(
  datS3_Count,
  file = "datS3_Count.csv",
  row.names = FALSE
)

datS3_Count <- read.csv("/Users/wildtype.human/bioinformatics/cFu/datS3_Count.csv")


combined_df <- left_join(datS3_Count, datS3, by = "Sample")

combined_df <- combined_df %>%
  rename(CFU_Count = CFU_per_mL) %>%
  select(Sample, Dilution, Count, CFU_Count, Cq, Quantity, CFU_TaqMan)

cFu_AN_20241612_TaqAndCount <- combined_df %>%
  mutate(Date_Collected = "20241612", Collected_By = "AN")

# Export as CSV
write.csv(
  cFu_AN_20241612_TaqAndCount,
  file = "cFu_AN_20241612_TaqAndCount.csv",
  row.names = FALSE
)

  