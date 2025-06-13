require(ggplot2)
require(tidyverse)

#Load and pivot data

datzw135C <- read.csv("/Users/wildtype.human/bioinformatics/cFu/cFu_WZ_20250513_RawCount.csv")

datzw135C <- datzw135C %>%
  pivot_longer(
    cols = -Sample,
    names_to = "Dilution",
    values_to = "Count"
  )

#Calculate CFU Counts, rename

cFu_ZW_20250513_Count <- datzw135C %>%
  mutate(
    CFU_Count = case_when(
      str_starts(Dilution, "N0") ~ Count * 1 * 50,
      str_starts(Dilution, "N1") ~ Count * 10 * 50,
      str_starts(Dilution, "N2") ~ Count * 100 * 50,
      TRUE ~ NA_real_
    )
  )