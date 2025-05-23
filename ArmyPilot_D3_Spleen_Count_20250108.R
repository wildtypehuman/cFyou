library(ggplot2)
library(tidyr)
library(dplyr)


datC <- read.csv("/Users/wildtype.human/bioinformatics/cFu/Army_Prelim_CFU_Counts.csv")

datS3_Count <- datC %>%
  filter(Count > 1, Time_Point == "Day 3", Organ == "Spleen")

datS3_Count <- datS3_Count %>%
  mutate(
    Count = as.numeric(Count), 
    CFU_Count = Count * 50 * case_when(
      Dilution == "N2" ~ 100,
      Dilution == "N3" ~ 1000,
      Dilution == "N4" ~ 10000,
      TRUE ~ 1 # Default multiplier is 1 if Dilution is not "N6" or "N7"
    )
  )





  