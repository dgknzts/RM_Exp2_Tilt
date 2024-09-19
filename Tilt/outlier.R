library(tidyverse)
library(ggplot2)
library(readxl)

# Set your working directory (modify as needed)
setwd("D:/Projects/RM_tilt/Tilt")

# Load the dataset
df <- read_xlsx('df_tilt.xlsx') %>% filter(correct_num == 3, RM != "Overestimate")
df_left_right <- df %>%
  group_by(SubID, ori_deg) %>%
  summarise(
    left_count = sum(rotation_response.keys == "left"),
    right_count = sum(rotation_response.keys == "right"),
    abs_difference = abs(left_count - right_count)
  )

ggplot(df_left_right, aes(x = ori_deg, y = right_count, group= ori_deg)) +
  geom_boxplot()


df_left_right_outliers <- df_left_right %>%
  group_by(ori_deg) %>%
  mutate(
    Q1 = quantile(left_count, 0.25, na.rm = TRUE),
    Q3 = quantile(left_count, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR,
    is_outlier = left_count < Lower_Bound | left_count > Upper_Bound
  ) %>%
  filter(is_outlier == TRUE)


# outlier based on performance -----------------------------
df_perf <- df %>%
  group_by(SubID, ori_deg, RM) %>%
  summarise(
    perf = mean(as.integer(ori_correct))
  )

ggplot(df_perf, aes(x = ori_deg, y = perf, group = ori_deg)) +
  geom_boxplot() +
  facet_wrap(~RM, nrow = 2) +
  scale_x_continuous(breaks = c(-10,-8,-6,-4,-2,0,2, 4, 6, 8, 10)) +
  theme_minimal()


df_perf_outliers <- df_perf %>%
  group_by(ori_deg, RM) %>%
  mutate(
    Q1 = quantile(perf, 0.25, na.rm = TRUE),
    Q3 = quantile(perf, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR,
    is_outlier = perf < Lower_Bound | perf > Upper_Bound
  ) %>%
  filter(is_outlier == TRUE) %>%
  ungroup() %>%
  summarise(SubID) %>%
  distinct()

ggplot(df_perf, aes(x = ori_deg, y = perf, group = ori_deg)) +
  geom_boxplot() +
  facet_wrap(~RM, nrow = 2) +
  scale_x_continuous(breaks = c(-10,-8,-6,-4,-2,0,2, 4, 6, 8, 10)) +
  theme_minimal()


