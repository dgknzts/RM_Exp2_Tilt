#plot without location of the tilted line:

summary_df_without_loc <- df %>%
  group_by(SubId, RM, amount, ori) %>%
  summarise(mean_number_deviation = mean(number_deviation),
            mean_ori = mean(as.numeric(ori_correct),
                            n = n()))


summary_df_all_without_loc <- df %>%
  group_by(amount, RM, ori) %>%
  summarise(mean_number_deviation = mean(number_deviation),
            mean_ori = mean(as.numeric(ori_correct)),
            n = n()) %>%
  ungroup()


ggplot() +
  geom_point(data = summary_df_without_loc, aes(x = as.factor(ori), y = mean_ori, color = SubId), position = position_dodge(width = 0.5), alpha = 0.2, size = 2) +
  labs(x = "Orientation Degree",
       y = "Orientation Correct Percentage",
       color = "Participant") +
  geom_point(data = summary_df_all_without_loc, aes(x = as.factor(ori), y = mean_ori, size = n), color = "darkblue", alpha = 0.8) +  # size ve alpha değerlerini artırıyoruz
  theme_minimal() +
  facet_wrap(~ amount + RM, scales = "free_x") +
  geom_text(data = summary_df_all_without_loc,
            aes(x = as.numeric(as.factor(ori)) + 0.001,
                y = mean_ori,
                label = round(mean_ori, 2)),
            hjust = 0, vjust = 0,
            color = "darkblue",
            alpha = 0.5)
