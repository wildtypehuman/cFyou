library(ggplot2)
library(tidyr)
library(dplyr)


datC <- read_csv("/Users/wildtype.human/bioinformatics/cFu/Army_Prelim_CFU_Counts.csv")

# Filter datC to only contain values of "Dilution" where "Count" is greater than 4

# Filter datC to only contain values where Time_Point = Input


# Filter datC to only contain rows where "Count" is greater than ?
datC <- datC %>%
  filter(Count > 0, Time_Point == "Input")

# Make a new column in datC and call it "CFU_per_mL", to get the value multiply count by 50 and if Diluton = N6, multiply 
# by 1000000 and if N7 by 10000000


datC <- datC %>%
  mutate(
    Count = as.numeric(Count), 
    CFU_per_mL = Count * 50 * case_when(
      Dilution == "N6" ~ 1000000,
      Dilution == "N7" ~ 10000000,
      TRUE ~ 1 # Default multiplier is 1 if Dilution is not "N6" or "N7"
    )
  )

# Box plot with default ggplot2 colors
ggplot(datC, aes(
  x = factor(Mtb_Strain), 
  y = CFU_per_mL, 
  fill = Mtb_Strain
)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "Input CFU Counts",
    x = "Input Type",
    y = "CFU (log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




