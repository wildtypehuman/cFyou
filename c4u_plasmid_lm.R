# Load required packages
require(ggplot2)
require(tidyverse)

# Create the data
datC4U <- tibble::tribble(
  ~Standard_Name, ~Standard_C,
  "S0",        3.82,
  "S1",         1.7,
  "S2",       0.767,
  "S3",       0.421,
  "S4",       0.177
)

# Calculate log concentration and index
datC4U <- datC4U %>%
  mutate(Log_Conc = log10(Standard_C),
         Index = 1:n())

# Fit the linear model
model <- lm(Log_Conc ~ Index, data = datC4U)

# Predict known standard concentrations
datC4U <- datC4U %>%
  mutate(Predicted_Log_Conc = predict(model, newdata = datC4U),
         Predicted_Conc = 10^Predicted_Log_Conc)

# Create new data points S5 to S17 for extrapolation
new_data <- tibble(
  Standard_Name = paste0("S", 5:17),
  Index = (nrow(datC4U) + 1):(nrow(datC4U) + 13)  # Continuing the index sequence
)

# Predict Log_Conc and Concentration for new values
new_data <- new_data %>%
  mutate(Predicted_Log_Conc = predict(model, newdata = new_data),
         Predicted_Conc = 10^Predicted_Log_Conc)

# Combine known and extrapolated values
final_data <- bind_rows(datC4U, new_data)

# Display final dataset
print(final_data)

# Optional: Plot the extrapolation
ggplot(final_data, aes(x = Index, y = Predicted_Conc)) +
  geom_point(aes(color = Standard_Name)) +
  geom_line() +
  scale_y_log10() +
  theme_minimal() +
  labs(title = "Extrapolated Concentrations",
       x = "Index",
       y = "Predicted Concentration (log scale)")

write.csv(final_data, "final_data.csv", row.names = FALSE)