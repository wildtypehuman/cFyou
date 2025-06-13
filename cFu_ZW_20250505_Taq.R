require(ggplot2)
require(tidyverse)

#Load raw data

datzw134 <- read_csv("/Users/wildtype.human/bioinformatics/cFu/cFu_ZW134_135.csv") %>%
  filter(str_detect(Sample, "134"))

#Filter and normalize, I bead beat 500 µL of each sampple, so no dilution

datzw134 <- datzw134 %>%
  filter(Task == "Unknown" & !is.na(Sample)) %>%
  mutate(
    Quantity = ifelse(is.na(Quantity), 0, as.numeric(Quantity)),
    CFU_TaqMan = (Quantity / 0.00478) / 0.002
  )
#Add metadata and rename


cFu_ZW_20250505_Taq <- datzw134 %>%
  mutate(Date_Collected = "20250505", Collected_By = "WZ")

#Export data

write.csv(cFu_ZW_20250505_Taq, file = "cFu_ZW_20250505_Taq.csv", row.names = FALSE)




