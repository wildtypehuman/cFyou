require(ggplot2)
require(tidyverse)

#Load raw data

datzw135 <- read_csv("/Users/wildtype.human/bioinformatics/cFu/cFu_ZW134_135.csv") %>%
  filter(str_detect(Sample, "135"))

#Filter and normalize taqman data, bead beat 500 µL of spleen lysate

datzw135 <- datzw135 %>%
  filter(Task == "Unknown" & !is.na(Sample)) %>%
  mutate(
    Quantity = ifelse(is.na(Quantity), 0, as.numeric(Quantity)),
    CFU_TaqMan = (Quantity / 0.00478) / 0.002
  )

#Add metadata and rename

cFu_WZ_20250513_Taq <- datzw135 %>%
  mutate(Date_Collected = "20250513", Collected_By = "WZ")

#Export final taqman data

write.csv(cFu_WZ_20250513_Taq , file = "cFu_WZ_20250513_Taq .csv", row.names = FALSE)






