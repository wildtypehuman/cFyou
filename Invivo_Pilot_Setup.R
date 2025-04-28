# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# Create simplified data frame
mouse_data <- data.frame(
  Group = c(1,2,3,4,5),
  Mouse_Count = c(3, 6, 6, 7, 7),  # Combined total mice per group
  Infection = c("PBS",
                "H37Rv",
                "H37Rv",
                "H37Rv + 10% BDQ res",
                "H37Rv + 50% BDQ res"),
  Treatment = c("None",
                "None",
                "Bedaquiline",
                "Bedaquiline",
                "Bedaquiline")
)

# Create grouped bar plot with ggplot2 default color palette
ggplot(mouse_data, aes(x=factor(Group), y=Mouse_Count, fill=Treatment)) +
  geom_bar(stat="identity", width=0.7) +
  geom_text(aes(label=Mouse_Count), position=position_stack(vjust=0.5), color="white", size=4) +
  theme_minimal() +
  labs(title="In vivo Screen Pilot Experiment",
       subtitle="Number of mice per group with infection type",
       x="Group",
       y="Number of Mice") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold")  # Bold x-axis labels
  ) +  
  # Add infection type labels below x-axis with bold text
  geom_text(aes(label=Infection, y=-0.5), size=2.9, angle=45, hjust=0, fontface = "bold") +
  # Adjust plot margins to accommodate angled labels
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.1)))

# Save plot
#ggsave("mouse_groups_default_palette.png", width=10, height=7)