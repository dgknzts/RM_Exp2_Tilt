library(readr)
library(dplyr)
library(readxl)
library(ggplot2)

df <- read_excel("df_tilt.xlsx")

df_hemisphere <- df %>%
  group_by(SubID, hemisphere, rotation_response.keys) %>% 
  summarise(n =  n())
  
  
#PLOT ORI CORRECTNESS JUST FOR 3 LINES
summary_df <- df %>%
  group_by(SubID, amount, ori_deg,oriented_line_location, hemisphere) %>%
  filter(RM != "Overestimate") %>%
  filter(amount == 3) %>%
  summarise(mean_number_deviation = mean(number_deviation),
            mean_ori = mean(as.numeric(ori_correct),
                            n = n()))


summary_df_all <- df %>%
  group_by(amount, ori_deg, oriented_line_location, hemisphere) %>%
  filter(RM != "Overestimate") %>%
  filter(amount == 3) %>%
  summarise(mean_number_deviation = mean(number_deviation),
            mean_ori = mean(as.numeric(ori_correct)),
            n = n())

ggplot() +
  geom_point(data = summary_df, aes(x = as.factor(ori_deg), y = mean_ori, color = SubID), position = position_dodge(width = 0.5), alpha = 0.2, size = 2) +
  labs(x = "Orientation Degree",
       y = "Orientation Correct Percentage",
       color = "Participant") +
  geom_point(data = summary_df_all, aes(x = as.factor(ori_deg), y = mean_ori, size = n), color = "darkblue", alpha = 0.8) + 
  #theme_minimal() +
  facet_wrap(hemisphere ~ oriented_line_location, scales = "free_x") +
  geom_text(data = summary_df_all,
            aes(x = as.numeric(as.factor(ori_deg)) + 0.001,
                y = mean_ori,
                label = round(mean_ori, 2)),
            hjust = 0, vjust = 0,
            color = "darkblue",
            alpha = 0.5)


