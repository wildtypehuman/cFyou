require(ggplot2)
require(tidyverse)
require(dplyr)
library(scales) 


datI <- read_csv("/Users/wildtype.human/bioinformatics/cFu/Input_Day1_Army_Pilot_2025-01-08-103234.csv")

# Filter datI to contain only values of Task = Unknown
# Add a new column called CFU_TaqMan, obtain CFU_TaqMan values by dividing Quantity by 0.00478, 
# then to get to CFU_TaqMan/ 1mL multiply by 0.002
# the the final Quantity value should be in CFU_TaqMan/mL
datI <- datI %>%
  filter(Task == "Unknown" & !is.na(Sample)) %>%
  mutate(CFU_TaqMan = (Quantity / 0.00478) / 0.002)

#Separate column Sample in datI into two columns, Sample and Type, new value of Sample will have everything before 
#the last underscore, and Type will have everything after the last underscore, can remove the last underscore form both columns

datI <- datI %>%
  mutate(
    Type = sub(".*_", "", Sample),        
    Sample = sub("_(?!.*_).*", "", Sample, perl = TRUE)  
  )
# Filter for Input, lung, or spleen
 datI <- datI %>%
   filter(Type == "Input") #%>%          
   # mutate(CFU_TaqMan = replace_na(CFU_TaqMan, 0))

# Make a new column Time_Point
 
 datI <- datI %>%
   mutate(Time_Point = "D_1")
 
 # Make a new column in datI and call it "Group", the value of Group are G2 and G3 (duplicate values), if Sample = H37Rv
 # if Sample = 10_BDQ_res, Group = G4, and if Sample = 50_BDQ_res, Group = G5
 
 datI <- datI %>%
   mutate(Group = case_when(
     Sample == "H37Rv" ~ "G2",
     Sample == "10_BDQ_res" ~ "G4",
     Sample == "50_BDQ_res" ~ "G5",
     TRUE ~ NA_character_
   )) %>%
   bind_rows(
     datI %>%
       filter(Sample == "H37Rv") %>%
       mutate(Group = "G3")
   )
 
#Input CFU boxplot

 ggplot(datI, aes(
  x = factor(Sample),
  y = CFU_TaqMan,
  fill = Sample
)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "Spleen Day 1 CFU from TaqMan assuming 0.00000478ng per cell (Log Scale)",
    x = "Sample",
    y = "CFU_TaqMan (per mL, log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

 ggplot(datI, aes(
   x = factor(Sample),
   y = CFU_TaqMan,
   fill = Sample
 )) +
   geom_boxplot() +
   scale_y_log10(
     limits = c(1e7, 1e9), # Specify the range for the y-axis
     breaks = trans_breaks("log10", function(x) 10^x), # Logarithmic breaks
     labels = trans_format("log10", math_format(10^.x)) # Logarithmic labels
   ) +
   theme_minimal() +
   labs(
     title = "Spleen Day 1 CFU from TaqMan assuming 0.00000478ng per cell (Log Scale)",
     x = "Sample",
     y = "CFU_TaqMan (per mL, log scale)"
   ) +
   theme(
     axis.text.x = element_text(angle = 45, hjust = 1)
   )


