#Extrapolate the unknown standard values using a regression model
require(ggplot2)
require(tidyverse)


datLM <- tibble::tribble(
  ~Sample_Name, ~Sample_Conc,
  "S1", 4.48,
  "S2", 1.7,
  "S3", 0.781,
  "S4", 0.278,
  "S5", 0.122
)

#Index helps with plotting
datLM <- datLM %>%
  mutate(Log_Conc = log10(Sample_Conc),
         Index = 1:n())


model <- lm(Log_Conc ~ Index, data = datLM)
summary(model)


datLM <- datLM %>%
  mutate(Predicted_Log_Conc = predict(model, newdata = datLM))


ggplot(datLM, aes(x = Index, y = Log_Conc)) +
  geom_point(size = 2) +                        
  geom_line(aes(y = Predicted_Log_Conc), size = 1) + 
  scale_x_continuous(breaks = datLM$Index, labels = datLM$Sample_Name) + # Use Sample_Name as x-axis labels
  labs(title = "Qbit DNA Standards",
       x = "Sample Name",
       y = "Log10 Concentration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = round(Log_Conc, 2)), vjust = -0.5, size = 3)




new_samples <- tibble(Sample_Name = paste0("S", 6:16),
                      Index = 6:16)


new_samples <- new_samples %>%
  mutate(Predicted_Log_Conc = predict(model, newdata = new_samples),
         Extrapolated_Conc = 10^Predicted_Log_Conc)

# Save the new_samples data frame as a CSV file
write.csv(new_samples, "new_samples.csv", row.names = FALSE)


new_samples <- tibble(Sample_Name = paste0("S", 1:16),
                      Index = 1:16)

new_samples <- new_samples %>%
  mutate(Predicted_Log_Conc = predict(model, newdata = new_samples),
         Extrapolated_Conc = 10^Predicted_Log_Conc)

# Save the new_samples data frame as a CSV file
write.csv(new_samples, "C4U_SD_LM.csv", row.names = FALSE)