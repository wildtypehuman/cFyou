#Day 3 Spleen TaqMan Data

require(ggplot2)
require(tidyverse)


datD3 <- read_csv("/Users/wildtype.human/bioinformatics/cFu/ArmyPilot_Day3_Taqman.csv")

# Filter datD3 to contain only values of Task = Unknown
# Add a new column called CFU_TaqMan, obtain CFU_TaqMan values by dividing Quantity by 0.00478, 
# then to get to CFU_TaqMan/ 1mL multiply by 0.002
# the the final Quantity value should be in CFU_TaqMan/mL

datD3 <- datD3 %>%
  filter(Task == "Unknown" & !is.na(Sample) & Sample != "DNASD_16") %>%
  mutate(CFU_TaqMan = (Quantity / 0.00478) / 0.002, CFU_TaqMan = replace_na(CFU_TaqMan, 0))



datD3 <- datD3 %>%
  mutate(Group = sub(".*_(.*)$", "\\1", Sample),  
         Sample = sub("^(.*)_.*$", "\\1", Sample))  

datD3 <- datD3 %>%
  mutate(Time_Point = "D_3")

datS3 <- datD3 %>%
  filter(grepl("^Spleen", Sample)) %>%
  select(-Group, -Time_Point)

#Add metadata and rename

cFu_AN_20241612_Taq <- datS3 %>%
  mutate(Date_Collected = "20241612", Collected_By = "AN")


# # Export as CSV
write.csv(
  cFu_AN_20241612_Taq,
  file = "cFu_AN_20241612_Taq.csv",
  row.names = FALSE
)

# 
# ggplot(datS3, aes(x = factor(Sample, levels = paste0("Spleen_", 1:10)), 
#                    y = CFU_TaqMan, 
#                    fill = Group)) + 
#   geom_boxplot() +
#   scale_y_log10() +
#   theme_minimal() +
#   labs(
#     title = "D3 Spleen CFU from TaqMan assuming 0.00000478ng per cell (Log Scale)",
#     x = "Sample",
#     y = "CFU_TaqMan (per mL, log scale)"
#   ) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )




# ggplot(datS3, aes(x = factor(Sample, levels = paste0("G", 2:5)), 
#                   y = CFU_TaqMan, 
#                   fill = Group)) + 
#   geom_boxplot() +
#   scale_y_log10() +
#   theme_minimal() +
#   labs(
#     title = "D3 Spleen CFU from TaqMan assuming 0.00000478ng per cell (Log Scale)",
#     x = "Group #",
#     y = "CFU_TaqMan (per mL, log scale)"
#   ) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )