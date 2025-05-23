library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(stringr)

datWZ_Count <- read.csv("/Users/wildtype.human/bioinformatics/cFu/cFu_WZ121124_Counts.csv")

#Pivot Longer
datWZ_Count <- datWZ_Count %>%
  pivot_longer(
    cols = -Sample,
    names_to = "Dilution",
    values_to = "Count"
  )
#Weihao plates 50uL per 12 wellplate, so transform per mL

cFu_WZ_20241211_Count <- datWZ_Count %>%
  mutate(
    CFU_Count = case_when(
      str_starts(Dilution, "N2") ~ Count * 100 * 20,
      str_starts(Dilution, "N3") ~ Count * 1000 * 20,
      TRUE ~ NA_real_
    )
  )

# add metadata
cFu_WZ_20241211_Count <- cFu_WZ_20241211_Count  %>%
  mutate(
Date_Collected = "20241211",
Collected_By = "ZW",
Experiment_ID = "zw126",
Date_Plated = "Wednesday,_December_11,_2024",
Date_Read = NA,
MTB_Strain_Type = "Rv_pMv261_zsGreen",
Strain_Modifications = "none",
Infection_Route = "Aerosol",
Mouse_Type = "B6_and_SP140ko",
Treatment = "none",
Organ_Tissue = "Spleen_mLN",
Sample_Storage = "Frozen_after_CFU",
Preprocessing = "Mash mLN in 6mL RPMI-5%FBS, mash spleen in 5mL PBST",
Tissue_Homogenization_Method = NA,
Timepoint_dpi = 29,
Log_Dilution_Factor = NA,
Media = "7H11",
Media_Additive = "none",
Plate_Type = "12_well",
Spreading_Method = NA,
Total_Plates = NA,
Number_of_Replicates = 3,
Volume_Per_Plate_uL = 50
)

write.csv(cFu_WZ_20241211_Count, file = "cFu_WZ_20241211_Count.csv", row.names = FALSE)



# ggplot(datWZ_Count, aes(x = Sample, y = CFU_Count)) +
#   geom_boxplot() +
#   scale_y_log10(labels = scientific_format()) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   ) +
#   labs(
#     y = "CFU Count (log scale)",
#     x = "Sample",
#     title = "CFU Counts by Sample and Dilution"
#   )
# 
# #combine count and taqman into one
# dat_WZ_combined <- left_join(dat_WZ_121124, datWZ_Count, by = "Sample")
# 
# dat_WZ_combined <- dat_WZ_combined %>%
#   select(-Well, -Omit, -Target, -Dye, -Task)
# 
# dat_WZ_combined <- dat_WZ_combined %>%
#   mutate(Date_Collected = "20241211", Collected_By = "WZ")
# 
# cFu_WZ_20241211_Taq_AndCount <- dat_WZ_combined
# 
# write.csv(
#   cFu_WZ_20241211_Taq_AndCount,
#   file = "cFu_WZ_20241211_Taq_AndCount.csv",
#   row.names = FALSE
# )

# ggplot(dat_WZ_combined, aes(x = CFU_Count, y = CFU_TaqMan)) +
#   geom_point(alpha = 0.7, size = 2) +
#   scale_x_log10(labels = scientific_format()) +
#   scale_y_log10(labels = scientific_format()) +
#   theme_minimal() +
#   labs(
#     x = "CFU Count (Plate Count, log scale)",
#     y = "CFU TaqMan (qPCR, log scale)",
#     title = "Comparison of CFU by Plate Count vs TaqMan"
#   )
# 
# #Probably need to average here
# 
# pearson_r <- cor(
#   log10(dat_WZ_combined$CFU_Count),
#   log10(dat_WZ_combined$CFU_TaqMan),
#   use = "complete.obs",
#   method = "pearson"
# )
# 
# ggplot(dat_WZ_combined, aes(x = CFU_Count, y = CFU_TaqMan)) +
#   geom_point(alpha = 0.7, size = 2) +
#   geom_smooth(method = "lm", se = FALSE, color = "blue") +
#   scale_x_log10(labels = scientific_format()) +
#   scale_y_log10(labels = scientific_format()) +
#   theme_minimal() +
#   labs(
#     x = "CFU Count (Plate Count, log scale)",
#     y = "CFU TaqMan (qPCR, log scale)",
#     title = "Pearson's Corr Plate Count and TaqMan CFU WZ_121124"
#   ) +
#   annotate("text",
#            x = min(dat_WZ_combined$CFU_Count, na.rm = TRUE),
#            y = max(dat_WZ_combined$CFU_TaqMan, na.rm = TRUE),
#            label = paste0("Pearson's r = ", round(pearson_r, 3)),
#            hjust = 0, vjust = 1, size = 5)
# 
# #Average first!
# 
# # Calculate average CFU values per Sample
# avg_df <- dat_WZ_combined %>%
#   filter(CFU_Count > 0, CFU_TaqMan > 0) %>%
#   group_by(Sample) %>%
#   summarise(
#     mean_CFU_Count = mean(CFU_Count, na.rm = TRUE),
#     mean_CFU_TaqMan = mean(CFU_TaqMan, na.rm = TRUE)
#   )
# 
# pearson_r <- cor(
#   log10(avg_df$mean_CFU_Count),
#   log10(avg_df$mean_CFU_TaqMan),
#   method = "pearson"
# )
# 
# ggplot(avg_df, aes(x = mean_CFU_Count, y = mean_CFU_TaqMan)) +
#   geom_point(size = 2) +
#   geom_smooth(method = "lm", se = FALSE, color = "blue") +
#   scale_x_log10(labels = scientific_format()) +
#   scale_y_log10(labels = scientific_format()) +
#   theme_minimal() +
#   labs(
#     x = "Mean CFU Count (Plate, log scale)",
#     y = "Mean CFU TaqMan (qPCR, log scale)",
#     title = "Mean CFU WZ_121124: Plate Count vs TaqMan"
#   ) +
#   annotate("text",
#            x = min(avg_df$mean_CFU_Count),
#            y = max(avg_df$mean_CFU_TaqMan),
#            label = paste0("Pearson's r = ", round(pearson_r, 3)),
#            hjust = 0, vjust = 1, size = 5)



