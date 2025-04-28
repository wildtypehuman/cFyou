# Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Create data frame for BDQ resistance categories
bdq_data <- data.frame(
  category = c(
    "Associated with R",
    "Associated with R - Interim",
    "Uncertain significance",
    "NOT Associated with R - Interim",
    "NOT Associated with R"
  ),
  count = c(0, 0, 533, 0, 4)
)

# Reorder categories for better visualization
bdq_data$category <- factor(
  bdq_data$category,
  levels = c(
    "Associated with R",
    "Associated with R - Interim",
    "Uncertain significance",
    "NOT Associated with R - Interim",
    "NOT Associated with R"
  )
)

# Create the visualization
ggplot(bdq_data, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity", width = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribution of BDQ (Bedaquiline) Resistance-Associated Variants",
    subtitle = "From M. tuberculosis Complex Mutation Catalogue",
    x = "Resistance Association Category",
    y = "Number of Variants",
    # caption = "Source: M. tuberculosis Complex Mutation Catalogue"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = count), vjust = -0.5, size = 4) +
  scale_y_continuous(limits = c(0, 600), expand = expansion(mult = c(0, 0.1)))