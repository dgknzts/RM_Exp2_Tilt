library(readr)
library(dplyr)
library(readxl)
library(ggplot2)

df <- read_excel("df_tilt.xlsx")




#PLOT ORI CORRECTNESS JUST FOR 3 LINES
summary_df <- df %>%
  filter(SubID != '505' ) %>% #fail control exp
  filter(amount == 3) %>%
  group_by(SubID, ori_deg, oriented_line_location) %>%
  summarise(mean_number_deviation = mean(number_deviation),
            #mean_ori = mean(as.numeric(ori_correct),
            n = n())


summary_df_all <- df %>%
  filter(SubID != '505' ) %>% #fail control exp
  filter(amount == 3) %>%
  group_by(ori_deg, oriented_line_location) %>%
  summarise(mean_number_deviation = mean(number_deviation),
            #mean_ori = mean(as.numeric(ori_correct)),
            n = n())


ggplot() +
  geom_point(data = summary_df, aes(x = as.factor(ori_deg), y = mean_number_deviation, color = SubID), position = position_dodge(width = 0.5), alpha = 0.2, size = 2) +
  labs(x = "Orientation Degree",
       y = "Mean Number Deviations",
       color = "Participant") +
  geom_point(data = summary_df_all, aes(x = as.factor(ori_deg), y = mean_number_deviation, size = n), color = "darkblue", alpha = 0.5) +  # size ve alpha değerlerini artırıyoruz
  #theme_minimal() +
  facet_wrap( ~ oriented_line_location, scales = "free_x", ncol = 3) +
  geom_text(data = summary_df_all,
            aes(x = as.numeric(as.factor(ori_deg)) + 0.001,
                y = mean_number_deviation,
                label = round(mean_number_deviation, 2)),
            hjust = 0, vjust = 0,
            color = "darkblue",
            alpha = 0.5)








