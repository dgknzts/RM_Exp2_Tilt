df_tilt_control %>% ggplot(aes(x = "", y = correct_percent)) +
  geom_boxplot()
  

df_cont_outliers <- df_tilt_control %>%
  mutate(
    Q1 = quantile(correct_percent, 0.25, na.rm = TRUE),
    Q3 = quantile(correct_percent, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR
  ) %>%
  filter(correct_percent >= Lower_Bound & correct_percent <= Upper_Bound) %>%
  ungroup()