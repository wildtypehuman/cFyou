library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)


datC <- read.csv("/Users/wildtype.human/bioinformatics/cFu/Army_Prelim_CFU_Counts.csv")

# Filter datC to only contain values of "Dilution" where "Count" is greater than 4

# Filter datC to only contain values where Time_Point = Input


# Filter datC to only contain rows where "Count" is greater than ?
# Filter for whatever stuff you need
datL1 <- datC %>%
  filter(Count > 1, Time_Point == "Day 1", Organ == "Lung", Dilution == "N0")



datL1 <- datL1 %>%
  mutate(
    Count = as.numeric(Count), 
    CFU_per_mL = Count * 50 * case_when(
      Dilution == "N0" ~ 1,
      Dilution == "N3" ~ 1000,
      TRUE ~ 1 # Default multiplier is 1 if Dilution is not "N6" or "N7"
    )
  )

# Box plot with default ggplot2 colors
ggplot(datL1, aes(
  x = factor(Mtb_Strain), 
  y = CFU_per_mL, 
  fill = Mtb_Strain
)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "Lung Day1 CFU Counts",
    x = "Input Type",
    y = "CFU (log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggplot(datL1, aes(
  x = factor(Time_Point), 
  y = CFU_per_mL, 
  fill = Time_Point
)) +
  geom_boxplot() +
  stat_summary(
    fun = median, 
    geom = "text", 
    aes(label = round(..y.., 2)), 
    vjust = -0.5, 
    color = "black"
  ) +
  scale_y_log10(
    limits = c(1e1, 1e5), # Set range for the y-axis
    breaks = trans_breaks("log10", function(x) 10^x), # Logarithmic breaks
    labels = trans_format("log10", math_format(10^.x)) # Logarithmic labels
  ) +
  theme_minimal() +
  labs(
    title = "Lung Day1 CFU Counts",
    x = "",
    y = "CFU (log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )