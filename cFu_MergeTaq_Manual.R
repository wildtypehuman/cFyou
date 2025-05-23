library(tidyverse)

df1 <- read.csv("/Users/wildtype.human/bioinformatics/cFu/Taq CSVs /cFu_AM_20240912_Taq.csv")
df2 <- read.csv("/Users/wildtype.human/bioinformatics/cFu/Taq CSVs /cFu_AM_20241219_Taq.csv")
df3 <- read.csv("/Users/wildtype.human/bioinformatics/cFu/Taq CSVs /cFu_AM_20250109_Taq.csv")
df4 <- read.csv("/Users/wildtype.human/bioinformatics/cFu/Taq CSVs /cFu_AN_20241612_Taq.csv")
df5 <- read.csv("/Users/wildtype.human/bioinformatics/cFu/Taq CSVs /cFu_RB_20240312_Taq.csv")
df6 <- read.csv("/Users/wildtype.human/bioinformatics/cFu/Taq CSVs /cFu_WZ_20241211_Taq.csv")
df7 <- read.csv("/Users/wildtype.human/bioinformatics/cFu/Taq CSVs /cFu_ZH_20241015_Taq.csv")



# Drop the Cq column from each data frame if it exists
dfs <- list(df1, df2, df3, df4, df5, df6, df7) %>%
  map(~ if ("Cq" %in% names(.x)) select(.x, -Cq) else .x)

# Optionally add a Source_File column to track origin
file_names <- c(
  "cFu_AM_20240912_Taq",
  "cFu_AM_20241219_Taq",
  "cFu_AM_20250109_Taq",
  "cFu_AN_20241612_Taq",
  "cFu_RB_20240312_Taq",
  "cFu_WZ_20241211_Taq",
  "cFu_ZH_20241015_Taq"
)

# Add file name as Source_File column
dfs_named <- map2(dfs, file_names, ~ mutate(.x, Source_File = .y))

# Combine into one data frame
taqman_combined <- bind_rows(dfs_named)