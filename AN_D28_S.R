#Analyze the D28 Spleen Data

require(ggplot2)
require(tidyverse)


datS28 <- read_csv("/Users/wildtype.human/bioinformatics/cFu/AN_D28_Spleens_2024-12-30-163945.csv")

# Filter datD28 to contain only values of Task = Unknown
# Add a new column called CFU_TaqMan, obtain CFU_TaqMan values by dividing Quantity by 0.00478, 
# then to get to CFU_TaqMan/ 1mL multiply by 0.002
# the the final Quantity value should be in CFU_TaqMan/mL
datS28 <- datS28 %>%
  filter(Task == "Unknown" & !is.na(Sample)) %>%
  mutate(CFU_TaqMan = (Quantity / 0.00478) / 0.002, CFU_TaqMan = replace_na(CFU_TaqMan, 0))

# Add "Group" column and separate "Sample" into "Group" and "Sample"
datS28 <- datS28 %>%
  mutate(Group = sub("_.*", "", Sample),  # Extract everything before the underscore
         Sample = sub(".*_", "", Sample))  # Extract everything after the underscore


datS28 <- datS28 %>%
  mutate(Time_Point = "D_28")


ggplot(datS28, aes(x = factor(Sample, levels = paste0("S", 1:4)), 
                   y = CFU_TaqMan, 
                   color = Group)) +  # Add fill for grouping by "Group"
  geom_boxplot() +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "D28 Spleen CFU from TaqMan assuming 0.00000478ng per cell (Log Scale)",
    x = "Sample",
    y = "CFU_TaqMan (per mL, log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )