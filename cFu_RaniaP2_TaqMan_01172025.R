#Rani's Samples from Dec 2024, Plate 2

library(dplyr)
library(ggplot2)
library(scales)

dat_RP2 <- read.csv("/Users/wildtype.human/bioinformatics/cFu/Rania_P2_01172025.csv")

# Choose only the samples I need, account for ng dna per bac, times 2 to account for dilution, and convert to 
# per 1 mL

dat_RP2 <- dat_RP2 %>%
  filter(Task == "Unknown" & !is.na(Sample)) %>%
  mutate(CFU_TaqMan = (Quantity / 0.00478) * 2 / 0.002)

datR <- bind_rows(dat_RP1, dat_RP2) 


datR <- datR %>%
  mutate(
    Group = sub("^(.*)_.*$", "\\1", Sample),  # Extract text before the underscore
    Sample = sub(".*_(.*)$", "\\1", Sample)   # Extract text after the underscore
  ) 

datR$Sample <- factor(
  datR$Sample,
  levels = sort(unique(datR$Sample))
)


datR$Group <- factor(
  datR$Group,
  levels = c("PBS", "IgG1", "36", "136")  
)


ggplot(datR, aes(x = Sample, y = CFU_TaqMan, fill = Group)) +  # Color by Group
  geom_boxplot() +
  scale_y_log10() +  # Log scale for y-axis
  theme_minimal() +
  labs(
    title = "RB CFU from TaqMan assuming 0.00000478ng per cell (Log Scale)",
    x = "Sample",
    y = "CFU_TaqMan (per mL, log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 

ggplot(datR, aes(x = Group, y = CFU_TaqMan, fill = Group)) +  # Color by Group
  geom_boxplot() +
  scale_y_log10() +  # Log scale for y-axis
  theme_minimal() +
  labs(
    title = "RB CFU from TaqMan assuming 0.00000478ng per cell (Log Scale)",
    x = "Treatment",
    y = "CFU_TaqMan (per mL, log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 


