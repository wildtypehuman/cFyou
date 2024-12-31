
#Just plot what zach came up with
require(ggplot2)
require(tidyverse)


# 
# ZH_counts <- tibble::tribble(
#                      ~Sample,     ~CFU_mL,
#                 "ZH_Oct15_1", 12333333.33,
#                 "ZH_Oct15_2",    38750000,
#                 "ZH_Oct15_3",     1.1e+08,
#                 "ZH_Oct15_4", 4166666.667,
#                 "ZH_Oct15_5", 33333333.33,
#                 "ZH_Oct15_6", 31666666.67,
#                 "ZH_Oct15_7", 2133333.333,
#                 "ZH_Oct15_8", 76666666.67,
#                 "ZH_Oct15_9",     1.3e+08,
#                "ZH_Oct15_10", 21666666.67,
#                "ZH_Oct15_11", 9333333.333,
#                "ZH_Oct15_12",           0,
#                "ZH_Oct15_13", 23333333.33,
#                "ZH_Oct15_14", 31666666.67,
#                "ZH_Oct15_15",       2e+07,
#                "ZH_Oct15_16",       6e+06,
#                "ZH_Oct15_17",     1.7e+08,
#                "ZH_Oct15_18", 13333333.33,
#                "ZH_Oct15_19", 11166666.67,
#                "ZH_Oct15_20", 41666666.67,
#                "ZH_Oct15_21", 23333333.33,
#                "ZH_Oct15_22", 633333.3333
#                )
# 
# 
# ggplot(ZH_counts, aes(x = factor(Sample, levels = paste0("ZH_Oct15_", 1:22)), y = CFU_mL)) +
#   geom_boxplot() +
#   scale_y_log10() +
#   theme_minimal() +
#   labs(
#     title = "ZH CFU from colony counts (Log Scale)",
#     x = "Sample",
#     y = "CFU (per mL, log scale)"
#   ) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )


RawZHdat <- tibble::tribble(
                    ~Sample,    ~Group, ~Dilution_counted, ~CFU_count_1, ~CFU_count_2, ~CFU_count_3,
               "ZH_Oct15_1", "Group_1",              "N4",          22L,          23L,          29L,
               "ZH_Oct15_2", "Group_1",              "N4",          69L,          86L,           NA,
               "ZH_Oct15_3", "Group_1",              "N5",           9L,          27L,          30L,
               "ZH_Oct15_4", "Group_1",              "N4",           8L,           9L,           8L,
               "ZH_Oct15_5", "Group_1",              "N5",           7L,           8L,           5L,
               "ZH_Oct15_6", "Group_1",              "N5",           2L,           5L,          12L,
               "ZH_Oct15_7", "Group_1",              "N3",          51L,          37L,          40L,
               "ZH_Oct15_8", "Group_2",              "N5",          17L,           9L,          20L,
               "ZH_Oct15_9", "Group_2",              "N5",          28L,          26L,          24L,
              "ZH_Oct15_10", "Group_2",              "N5",           3L,           3L,           7L,
              "ZH_Oct15_11", "Group_2",              "N4",          24L,          21L,          11L,
              "ZH_Oct15_12", "Group_2",                NA,           0L,           0L,           0L,
              "ZH_Oct15_13", "Group_2",              "N5",           7L,           4L,           3L,
              "ZH_Oct15_14", "Group_2",              "N5",           5L,           5L,           9L,
              "ZH_Oct15_15", "Group_2",              "N5",           4L,           4L,           4L,
              "ZH_Oct15_16", "Group_3",              "N4",           8L,          11L,          17L,
              "ZH_Oct15_17", "Group_3",              "N5",          26L,          39L,          34L,
              "ZH_Oct15_18", "Group_3",              "N5",           4L,           2L,           2L,
              "ZH_Oct15_19", "Group_3",              "N4",          19L,          23L,          25L,
              "ZH_Oct15_20", "Group_3",              "N5",           4L,          14L,           7L,
              "ZH_Oct15_21", "Group_3",              "N5",           5L,           7L,           2L,
              "ZH_Oct15_22", "Group_3",              "N3",          11L,          16L,          11L
              )


RawZHdat <- RawZHdat %>%
  pivot_longer(
    cols = starts_with("CFU_count"),      # Select columns to pivot
    names_to = "Replicate",               # New column for the names
    values_to = "CFU_count"                     # New column for the values
  ) %>%
  # add a column called CFU_count_per_mL, to obtain CFU_count_per_mL value, take CFU_count * 50 * 1000 if 
  # Dilution_counted = N3, 10000 if " = N4, and 100000 if " = N5
  mutate(CFU_count_per_mL = case_when(
    Dilution_counted == "N3" ~ CFU_count * 50 * 1000,
    Dilution_counted == "N4" ~ CFU_count * 50 * 10000,
    Dilution_counted == "N5" ~ CFU_count * 50 * 100000,
    TRUE ~ NA_real_  # For cases where Dilution_counted is not N3, N4, or N5
  ))

ggplot(RawZHdat, aes(x = factor(Sample, levels = paste0("ZH_Oct15_", 1:22)), y = CFU_count_per_mL)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "ZH CFU counts (Log Scale)",
    x = "Sample",
    y = "CFU counts (per mL, log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


datZH <- datZH %>%
  select(Sample, CFU_TaqMan)

# Merge RawZHdat with the selected columns from datZH by the 'Sample' column
ZHall <- RawZHdat %>%
  left_join(datZH, by = "Sample")


# ICC, dot plot of ZHall with CFU_count_per_mL on the x axis and CFU_TaqMan on the 
# y axis, log both axis and fit a line with an ICC coefficient


# Step 2: Dot plot with log-log axes and a regression line
ggplot(ZHall, aes(x = CFU_count_per_mL, y = CFU_TaqMan)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", col = "blue", se = FALSE) +
  labs(
    title = "Dot Plot of Colony Counts vs TaqMan Measurements",
    x = "Colony Counts (CFU per mL, log scale)",
    y = "TaqMan Counts (CFU per mL, log scale)"
  ) +
  theme_minimal()

library(psych)

icc_result <- ICC(ZHall %>% select(CFU_count_per_mL, CFU_TaqMan))

print(icc_result)

cor_result <- cor(ZHall$CFU_count_per_mL, ZHall$CFU_TaqMan, use = "complete.obs")

# Print the correlation coefficient
print(paste("Pearson correlation coefficient:", round(cor_result, 3)))






ZHall_long <- ZHall %>%
  pivot_longer(
    cols = c(CFU_count_per_mL, CFU_TaqMan),
    names_to = "Method",
    values_to = "CFU"
  )

# Step 2: Set the order of the Sample column
ZHall_long$Sample <- factor(ZHall_long$Sample, levels = paste0("ZH_Oct15_", 1:22))
# Step 3: Create the box plot with differentiation by Group and Method
ggplot(ZHall_long, aes(x = Sample, y = CFU, fill = Method, color = Group)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +  # Dodge box plots side by side
  scale_y_log10() +  # Log scale for better visualization
  labs(
    title = "Comparison of Colony Counts vs TaqMan Measurements by Sample and Group",
    x = "Sample",
    y = "CFU (log scale)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  ) +
  scale_fill_brewer(palette = "Set2") +  # Color palette for Method
  scale_color_manual(values = c("Group_1" = "red", "Group_2" = "blue", "Group_3" = "green"))  # Colors for Groups





