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

# Change dat to add Sample column, so I can merge taqman and count
write.csv(
  datS3_Count,
  file = "datS3_Count.csv",
  row.names = FALSE
)

datAN <- read.csv("/Users/wildtype.human/bioinformatics/cFu/cFu_AN_RawCounts.csv")


#add metadata

datAN <- datAN %>%
  mutate(
    Date_Collected = "20241612",
    Collected_By = "AN",
    Experiment_ID = "AN_111924",
    Date_Plated = "Saturday,_November_22,_2025",
    Date_Read = "Tuesday,_December_16,_2025",
    MTB_Strain_Type = "H37Rv",
    Strain_Modifications = "Rv0678_mut",
    Infection_Route = "retroorbital",
    Mouse_Type = "BALB/C",
    Treatment = "Bedaquiline",
    Organ_Tissue = "Lung_and_Spleen",
    Sample_Storage = "no",
    Preprocessing = "none",
    Tissue_Homogenization_Method = "mash_through_strainer",
    Timepoint_dpi = 3,
    Log_Dilution_Factor = NA,
    Media = "7H11",
    Media_Additive = "Charcoal",
    Plate_Type = "6_well",
    Spreading_Method = "turn",
    Total_Plates = NA,
    Number_of_Replicates = 3,
    Volume_Per_Plate_uL = 20
  )

# rename and export as csv

cFu_AN_20241612_Count <- datAN

write.csv(cFu_AN_20241612_Count, file = "cFu_AN_20241612_Count.csv", row.names = FALSE)


# combined_df <- left_join(datS3_Count, datS3, by = "Sample")
# 
# combined_df <- combined_df %>%
#   rename(CFU_Count = CFU_per_mL) %>%
#   select(Sample, Dilution, Count, CFU_Count, Cq, Quantity, CFU_TaqMan)
# 
# cFu_AN_20241612_TaqAndCount <- combined_df %>%
#   mutate(Date_Collected = "20241612", Collected_By = "AN")
# 
# # Export as CSV
# write.csv(
#   cFu_AN_20241612_TaqAndCount,
#   file = "cFu_AN_20241612_TaqAndCount.csv",
#   row.names = FALSE
# )

  