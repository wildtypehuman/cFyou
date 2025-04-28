# Do Passing-Bablok, Bland Altman, CCC.

library(dplyr)
library(readr)

# List of files
file_paths <- c(
  "/Users/wildtype.human/bioinformatics/cFu/CSV to merge into master/cFu_AM_20240912_TaqAndCounts.csv",
  "/Users/wildtype.human/bioinformatics/cFu/CSV to merge into master/cFu_AM_20241219_TaqAndCount.csv",
  "/Users/wildtype.human/bioinformatics/cFu/CSV to merge into master/cFu_AM_20250109_TaqAndCount.csv",
  "/Users/wildtype.human/bioinformatics/cFu/CSV to merge into master/cFu_AN_20241612_TaqAndCount.csv",
  "/Users/wildtype.human/bioinformatics/cFu/CSV to merge into master/cFu_RB_20240312_TaqAndCount.csv",
  "/Users/wildtype.human/bioinformatics/cFu/CSV to merge into master/cFu_WZ_20241211_TaqAndCount.csv",
  "/Users/wildtype.human/bioinformatics/cFu/CSV to merge into master/cFu_ZH_20241015_TaqAndCount.csv"
)

# Helper function to read and clean each file
read_and_clean <- function(file_path) {
  df <- read_csv(file_path, show_col_types = FALSE)
  
  df %>%
    mutate(
      # Make sure Dilution is character
      Dilution = as.character(Dilution),
      # Make sure Cq is numeric (suppress warning if there are text like "Undetermined")
      Cq = suppressWarnings(as.numeric(Cq)),
      # Rename CFU_per_mL to CFU_Count if needed
      CFU_Count = ifelse("CFU_per_mL" %in% names(.), CFU_per_mL, CFU_Count)
    ) %>%
    select(Sample, Dilution, Count, CFU_Count, Cq, Quantity, CFU_TaqMan)
}

# Apply function to all files
dfs_cleaned <- lapply(file_paths, read_and_clean)

# Bind them all together
master_df <- bind_rows(dfs_cleaned)

# Quick check
glimpse(master_df)


