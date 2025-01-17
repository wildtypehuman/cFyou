#Rani's Samples from Dec 2024, Plate 1 

library(dplyr)
library(ggplot2)
library(scales)

dat_RP1 <- read.csv("/Users/wildtype.human/bioinformatics/cFu/Rania_P1_01172025.csv")

# Choose only the samples I need, account for ng dna per bac, times 2 to account for dilution, and convert to 
# per 1 mL

dat_RP1 <- dat_RP1 %>%
  filter(Task == "Unknown" & !is.na(Sample)) %>%
  mutate(CFU_TaqMan = (Quantity / 0.00478) * 2 / 0.002)