library(ggplot2)
library(tidyr)
library(dplyr)
library(scales) 


datC <- read.csv("/Users/wildtype.human/bioinformatics/cFu/Army_Prelim_CFU_Counts.csv")

# Filter datC to only contain values of "Dilution" where "Count" is greater than 4

# Filter datC to only contain values where Time_Point = Input


# Filter datC to only contain rows where "Count" is greater than ?
# Filter for whatever stuff you need
datI <- datC %>%
  filter(Count > 0, Time_Point == "Input", Dilution == "N6")

# Make a new column in datC and call it "CFU_per_mL", to get the value multiply count by 50 and if Diluton = N6, multiply 
# by 1000000 and if N7 by 10000000


datI <- datI %>%
  mutate(
    Count = as.numeric(Count), 
    CFU_per_mL = Count * 50 * case_when(
      Dilution == "N6" ~ 1000000,
      Dilution == "N7" ~ 10000000,
      TRUE ~ 1 # Default multiplier is 1 if Dilution is not "N6" or "N7"
    )
  )

# Box plot with default ggplot2 colors
ggplot(datI, aes(
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

ggplot(datI, aes(
  x = factor(Time_Point), 
  y = CFU_per_mL, 
  fill = Time_Point
)) +
  geom_boxplot() +
  scale_y_log10(
    limits = c(1e7, 1e9), # Set range for the y-axis
    breaks = trans_breaks("log10", function(x) 10^x), # Logarithmic breaks
    labels = trans_format("log10", math_format(10^.x)) # Logarithmic labels
  ) +
  theme_minimal() +
  labs(
    title = "Input CFU Counts",
    x = "",
    y = "CFU (log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
library(ggplot2)
library(scales)

ggplot(datI, aes(
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
    limits = c(1e7, 1e9), # Set range for the y-axis
    breaks = trans_breaks("log10", function(x) 10^x), # Logarithmic breaks
    labels = trans_format("log10", math_format(10^.x)) # Logarithmic labels
  ) +
  theme_minimal() +
  labs(
    title = "Input CFU Counts Per mL",
    x = "",
    y = "CFU (log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

