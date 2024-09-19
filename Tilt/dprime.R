library(tidyverse)
library(ggplot2)
library(readxl)

# Set your working directory (modify as needed)
setwd("D:/Projects/RM_tilt/Tilt")

# Load the dataset
df <- read_xlsx('df_tilt.xlsx')

# d-prime calculation
df_d <- df %>%
  filter(correct_num == 3) %>%
  mutate(
    ori_abs = abs(ori_deg),
    dprime = case_when(
      direction == "right" & rotation_response.keys == "right" ~ "Hit",
      direction == "right" & rotation_response.keys == "left" ~ "Miss",
      direction == "left" & rotation_response.keys == "right" ~ "FA",
      direction == "left" & rotation_response.keys == "left" ~ "CorRej"
    )
  ) %>%
  group_by(SubID, ori_abs, RM) %>%
  summarise(
    n = n(),
    Hits = sum(dprime == "Hit"),
    Misses = sum(dprime == "Miss"),
    False_Alarms = sum(dprime == "FA"),
    Correct_Rejections = sum(dprime == "CorRej"),
    
    # Calculate hit and false alarm rates
    hit_rate = ifelse(Hits + Misses > 0, Hits / (Hits + Misses), NA),
    fa_rate = ifelse(False_Alarms + Correct_Rejections > 0, False_Alarms / (False_Alarms + Correct_Rejections), NA),
    
    # Adjust hit rate and false alarm rate, handling cases where Hits + Misses or False Alarms + Correct Rejections are 0
    hit_rate_adj = ifelse(is.na(hit_rate), 0.5 / (Hits + Misses + 1), 
                          ifelse(hit_rate == 0, 0.5 / (Hits + Misses), 
                                 ifelse(hit_rate == 1, 1 - 0.5 / (Hits + Misses), hit_rate))),
    fa_rate_adj = ifelse(is.na(fa_rate), 0.5 / (False_Alarms + Correct_Rejections + 1), 
                         ifelse(fa_rate == 0, 0.5 / (False_Alarms + Correct_Rejections), 
                                ifelse(fa_rate == 1, 1 - 0.5 / (False_Alarms + Correct_Rejections), fa_rate))),
    
    # Calculate d-prime and criterion
    dprime_value = qnorm(hit_rate_adj) - qnorm(fa_rate_adj),
    criterion = -0.5 * (qnorm(hit_rate_adj) + qnorm(fa_rate_adj))
  )



# Plotting the d-prime values
ggplot(df_d, aes(x = ori_abs, y = dprime_value, group = ori_abs)) +
  geom_boxplot() +
  facet_wrap(~RM, ncol = 3) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  labs(title = "Boxplot of d-prime Values by RM and Orientation", 
       x = "Absolute Orientation (Degrees)", 
       y = "d-prime Value") +
  theme_minimal()

# Plotting d-prime values AFTER THE ELIMINATION -----------------

df_d_eliminated <- df_d %>%
  mutate(
    Q1 = quantile(dprime_value, 0.25, na.rm = TRUE),
    Q3 = quantile(dprime_value, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR
  ) %>%
  filter(dprime_value >= Lower_Bound & dprime_value <= Upper_Bound) %>%
  ungroup()


ggplot(df_d_eliminated, aes(x = ori_abs, y = dprime_value, group = ori_abs)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  facet_wrap(~oriented_line_location, ncol = 3) +
  scale_x_continuous(breaks = c(2,4,6,8,10)) +
  theme_minimal()
  
  
