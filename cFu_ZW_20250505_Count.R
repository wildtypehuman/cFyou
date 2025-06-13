require(ggplot2)
require(tidyverse)

#Load and pivot count data
datzw134C <- read.csv("/Users/wildtype.human/bioinformatics/cFu/cFu_ZW_20250505_RawCount.csv")

datzw134C <- datzw134C %>%
  pivot_longer(
    cols = -Sample,
    names_to = "Dilution",
    values_to = "Count"
  )

#Calculate cfu counts and rename

cFu_ZW_20250505_Count <- datzw134C %>%
  mutate(
    CFU_Count = case_when(
      str_starts(Dilution, "N1") ~ Count * 10 * 50,
      str_starts(Dilution, "N2") ~ Count * 100 * 50,
      TRUE ~ NA_real_
    )
  )

#Add metadata








