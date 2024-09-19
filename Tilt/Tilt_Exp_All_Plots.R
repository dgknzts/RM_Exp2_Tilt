#---libraries--####
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(rstatix)


#----parameters before run --####

set_size = 3 # or 4 -- selecting Set Size to inspect

failed_list <- c("505", "513", "516", "517", "518", "525") #505 has %60 Correctness -> df_tilt_control.xlsx

participant_list  <- c(as.character(seq(501, 530, 1))) #participant code list
participant_list <- participant_list[!participant_list %in% failed_list] #participant code list after elimination

# a theme for each plot
theme_my_data <- theme(
  axis.title.x = element_text(
    color = "black",
    size = 14,
    face = "bold"
  ),
  axis.title.y = element_text(
    color = "black",
    size = 14,
    face = "bold"
  ),
  panel.border = element_blank(),
  # remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # remove panel background
  panel.background = element_blank(),
  # add axis line
  axis.line = element_line(colour = "grey"),
  # x,y axis tick labels
  axis.text.x = element_text(size = 12, face = "bold"),
  axis.text.y = element_text(size = 12, face = "bold"),
  # legend size
  legend.title = element_text(size = 12, face = "bold"),
  legend.text = element_text(size = 10),
  # facet wrap title
  strip.text.x = element_text(size = 12, face = "bold")
)


#---load data--####
setwd("D:/Projects/RM_Tilt/Tilt") #working directory

my_data <- read_excel("df_tilt.xlsx") #load data

my_data <- my_data %>%
  filter(!SubID %in% failed_list) #filter out the low performance participants on control exp

#filtering data based on the parameter we choose and set it as main data to plot
my_data_3 <- my_data %>% filter(correct_num == 3)
my_data_4 <- my_data %>% filter(correct_num == 4)

if (set_size == 3) {
  df <- my_data_3
  nrow <- 3
} else {
  df <- my_data_4
  nrow <- 4
} 


# RM summary
RM_summary <- df %>%
  group_by(RM) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage_RM = (count / sum(count)) * 100)

#----NUMBER DEVIATION VS ORI DEGREE by LOCATION -- ####

data_by_pp <- df %>%
  group_by(
    SubID,
    ori_deg,
    oriented_line_location
    ) %>%
  summarise(
    num_deviation_mean = mean(number_deviation),
    num_deviation_sd = sd(number_deviation),
    n = n()
  ) %>%
  mutate(
    num_deviation_SEM = num_deviation_sd / sqrt(n),
    num_deviation_CI = num_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


data_across_pp <- data_by_pp %>%
  group_by(
    ori_deg,
    oriented_line_location
    ) %>%
  summarise(
    num_dv_mean = mean(num_deviation_mean),
    num_dv_sd = sd(num_deviation_mean),
    n = n()
    ) %>%
  mutate(
    num_dv_SEM = num_dv_sd / sqrt(n),
    num_dv_CI = num_dv_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

#adding levels to subID to be able to order
data_by_pp$SubID <-
  factor(
    data_by_pp$SubID,
    levels = participant_list
  )



my_plot1.0 <- ggplot() +
  
  geom_point(
    data = data_by_pp, 
    aes(
      x = as.factor(ori_deg),
      y = num_deviation_mean,
      color = SubID),
    position = position_dodge(width = 0.4),
    stat = 'identity',
    alpha = 0.4,
    size = 1) +
  
  
  labs(
    x = "Orientation Degree",
    y = "Mean Number Deviations",
    color = "Participant") +
  
  
  geom_point(
    data = data_across_pp,
    aes(x = as.factor(ori_deg),
        y = num_dv_mean),
    color = "black",
    alpha = 0.8,
    size = 4) +
  
  geom_errorbar(
    data = data_across_pp,
    aes(
      x = as.factor(ori_deg),
      y = num_dv_mean,
      ymin = num_dv_mean - num_dv_CI,
      ymax = num_dv_mean + num_dv_CI
    ),
    size  = 1.2,
    width = .00,
    alpha = 0.8,
    position = position_dodge(width = 0.5)
  ) +
  
  geom_text(
    data = data_across_pp,
    aes(x = as.numeric(as.factor(ori_deg)) + 0.001,
               y = num_dv_mean,
               label = round(num_dv_mean, 2)),
           hjust = 0, vjust = 0,
           color = "black",
           alpha = 1) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  facet_wrap( ~ oriented_line_location, nrow  = 3, labeller = labeller(
    oriented_line_location =
      c(
        "inner" = "Inner Line Tilted",
        "middle" = "Middle Line Tilted",
        "outer" = "Outer Line Tilted",
        "middle_inner" = "Middle-Inner Line Tilted",
        "middle_outer" = "Middle-Outer Line Tilted"
      ))
  ) +
    
    theme_my_data

my_plot1.0

#----NUMBER DEVIATION VS ORI DEGREE combined location -- ####
data_by_pp1.1 <- df %>%
  group_by(
    SubID,
    ori_deg
  ) %>%
  summarise(
    num_deviation_mean = mean(number_deviation),
    num_deviation_sd = sd(number_deviation),
    n = n()
  ) %>%
  mutate(
    num_deviation_SEM = num_deviation_sd / sqrt(n),
    num_deviation_CI = num_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


data_across_pp1.1 <- data_by_pp1.1 %>%
  group_by(
    ori_deg
  ) %>%
  summarise(
    num_dv_mean = mean(num_deviation_mean),
    num_dv_sd = sd(num_deviation_mean),
    n = n()
  ) %>%
  mutate(
    num_dv_SEM = num_dv_sd / sqrt(n),
    num_dv_CI = num_dv_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

#adding levels to subID to be able to order
data_by_pp1.1$SubID <-
  factor(
    data_by_pp1.1$SubID,
    levels = participant_list
  )



my_plot1.1 <- ggplot() +
  
  geom_point(
    data = data_by_pp1.1, 
    aes(
      x = as.factor(ori_deg),
      y = num_deviation_mean,
      color = SubID),
    position = position_dodge(width = 0.4),
    stat = 'identity',
    alpha = 0.4,
    size = 1) +
  
  
  labs(
    x = "Orientation Degree",
    y = "Mean Number Deviations",
    color = "Participant") +
  
  
  geom_point(
    data = data_across_pp1.1,
    aes(x = as.factor(ori_deg),
        y = num_dv_mean),
    color = "black",
    alpha = 0.8,
    size = 4) +
  
  geom_errorbar(
    data = data_across_pp1.1,
    aes(
      x = as.factor(ori_deg),
      y = num_dv_mean,
      ymin = num_dv_mean - num_dv_CI,
      ymax = num_dv_mean + num_dv_CI
    ),
    size  = 1.2,
    width = .00,
    alpha = 0.8,
    position = position_dodge(width = 0.5)
  ) +
  
  geom_text(
    data = data_across_pp1.1,
    aes(x = as.numeric(as.factor(ori_deg)) + 0.001,
        y = num_dv_mean,
        label = round(num_dv_mean, 2)),
    hjust = 0, vjust = 0,
    color = "black",
    alpha = 1) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  theme_my_data

my_plot1.1

#----NUMBER DEVIATION VS ORI DEGREE combined location WITH STAT_SMOOTH -- ####

my_plot1.2 <- ggplot() +
  
  geom_point(
    data = data_by_pp1.1, 
    aes(
      x = ori_deg,
      y = num_deviation_mean,
      color = SubID),
    position = position_dodge(width = 0.4),
    stat = 'identity',
    alpha = 0.4,
    size = 1) +
  
  
  geom_point(
    data = data_across_pp1.1,
    aes(x = ori_deg,
        y = num_dv_mean),
    color = "black",
    alpha = 0.8,
    size = 4) +
  
  
  geom_errorbar(
    data = data_across_pp1.1,
    aes(
      x = ori_deg,
      y = num_dv_mean,
      ymin = num_dv_mean - num_dv_CI,
      ymax = num_dv_mean + num_dv_CI
    ),
    size  = 1.2,
    width = .00,
    alpha = 0.8,
    position = position_dodge(width = 0.5)
  ) +
  
  
  geom_text(
    data = data_across_pp1.1,
    aes(x = as.numeric(ori_deg) + 0.001,
        y = num_dv_mean,
        label = round(num_dv_mean, 2)),
    hjust = 0, vjust = 0,
    color = "black",
    alpha = 1) + 
  
  
  stat_smooth(
    data = data_across_pp,
    aes(
      x = ori_deg,
      y = num_dv_mean
    ),
    method = "glm",
    size = 2,
    se = FALSE,
    alpha = 0.5,
    geom = "line",
    formula = y ~ poly(x, 2)
  ) +
  
  
  scale_x_continuous(breaks = seq(-10,10,2))+
  
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  
  labs(
    x = "Orientation Degree",
    y = "Mean Number Deviations",
    color = "Participant") +
  
  
  theme_my_data

my_plot1.2

#---- ORI CORRECT PERCENTAGES vs ORI DEG by LOCATION + RM-----#####

data_by_pp2 <- df %>%
  filter(RM %in% c('RM', "Correct")) %>%
  group_by(
    SubID,
    ori_deg,
    oriented_line_location,
    RM
  ) %>%
  summarise(
    ori_resp_mean = mean(as.integer(ori_correct)),
    ori_resp_sd = sd(as.integer(ori_correct)),
    n = n()
  ) %>%
  mutate(
    ori_resp_SEM = ori_resp_sd / sqrt(n),
    ori_resp_CI = ori_resp_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


data_across_pp2 <- data_by_pp2 %>%
  group_by(
    ori_deg,
    oriented_line_location,
    RM
  ) %>%
  summarise(
    ori_rp_mean = mean(ori_resp_mean),
    ori_rp_sd = sd(ori_resp_mean),
    n = n()
  ) %>%
  mutate(
    ori_rp_SEM = ori_rp_sd / sqrt(n),
    ori_rp_CI = ori_rp_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

#adding levels to subID to be able to order
data_by_pp2$SubID <-
  factor(
    data_by_pp2$SubID,
    levels = participant_list
  )



my_plot2.0 <- ggplot() +
  
  
  geom_point(
    data = data_by_pp2,
    aes(
      x = as.factor(ori_deg),
      y = ori_resp_mean,
      fill = RM,
      group = RM,
      color = RM,
      size = n
      ),
    position = position_dodge(width = 0.5),
    alpha = 0.1,
    show.legend = FALSE) +
  
  
  geom_point(
    data = data_across_pp2,
    aes(x = as.factor(ori_deg),
        y = ori_rp_mean,
        group = RM,
        color = RM,
        size = n),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE) +

  
  geom_errorbar(
    data = data_across_pp2,
    aes(
      x = as.factor(ori_deg),
      y = ori_rp_mean,
      group = RM,
      color = RM,
      ymin = ori_rp_mean - ori_rp_CI,
      ymax = ori_rp_mean + ori_rp_CI,
      
    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.9,
    position = position_dodge(0.5)
  ) +
  
  facet_wrap( ~ oriented_line_location, ncol = 3, labeller = labeller(
    oriented_line_location =
      c(
        "inner" = "Inner Line Tilted",
        "middle" = "Middle Line Tilted",
        "outer" = "Outer Line Tilted",
        "middle_inner" = "Middle-Inner Line Tilted",
        "middle_outer" = "Middle-Outer Line Tilted"
      ))
  ) +
  
  
  #geom_hline(yintercept = 0.5, linetype = "dashed") +

  
  scale_color_manual(
    labels = c("non-RM trials", "RM trials"),
    values = c( "#298C8C", "#800074"),
    name = ""
  ) +
  
  theme_my_data  +
  
  
  labs(
    x = "Orientation Degree",
    y = "Orientation Correct Percentage",
    color = "Redundancy Masking"
  )
  

my_plot2.0 



my_plot2.1 <- ggplot() +
  
  
  
  geom_point(
    data = data_across_pp2,
    aes(x = ori_deg,
        y = ori_rp_mean,
        group = RM,
        color = RM,
        size = n),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE) +
  
  
  geom_errorbar(
    data = data_across_pp2,
    aes(
      x = ori_deg,
      y = ori_rp_mean,
      group = RM,
      color = RM,
      ymin = ori_rp_mean - ori_rp_CI,
      ymax = ori_rp_mean + ori_rp_CI,
      
    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.9,
    position = position_dodge(0.5)
  ) +
  
  facet_wrap( ~ oriented_line_location, ncol = 3, labeller = labeller(
    oriented_line_location =
      c(
        "inner" = "Inner Line Tilted",
        "middle" = "Middle Line Tilted",
        "outer" = "Outer Line Tilted",
        "middle_inner" = "Middle-Inner Line Tilted",
        "middle_outer" = "Middle-Outer Line Tilted"
      ))
  ) +
  
  
  stat_smooth(
    data = data_across_pp2,
    aes(
      x = ori_deg,
      y = ori_rp_mean,
      group = RM,
      color = RM
    ),
    method = "glm",
    size = 2,
    se = FALSE,
    alpha = 0.5,
    geom = "line",
    formula = y ~ poly(x, 2)
    
  ) +
  
  
  scale_color_manual(
    labels = c("non-RM trials", "RM trials"),
    values = c( "#298C8C", "#800074"),
    name = ""
  ) +
  
  theme_my_data  +
  
  
  labs(
    x = "Orientation Degree",
    y = "Orientation Correct Percentage",
    color = "Redundancy Masking"
  )


my_plot2.1

#---- ORI CORRECT PERCENTAGES vs ORI DEG by LOCATION + RM + Visual Field -----#####

data_by_pp3 <- df %>%
  filter(RM %in% c('RM', "Correct")) %>%
  group_by(
    SubID,
    ori_deg,
    oriented_line_location,
    RM,
    hemisphere
  ) %>%
  summarise(
    ori_resp_mean = mean(as.integer(ori_correct)),
    ori_resp_sd = sd(as.integer(ori_correct)),
    n = n()
  ) %>%
  mutate(
    ori_resp_SEM = ori_resp_sd / sqrt(n),
    ori_resp_CI = ori_resp_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


data_across_pp3 <- data_by_pp3 %>%
  group_by(
    ori_deg,
    oriented_line_location,
    RM,
    hemisphere
  ) %>%
  summarise(
    ori_rp_mean = mean(ori_resp_mean),
    ori_rp_sd = sd(ori_resp_mean),
    n = n()
  ) %>%
  mutate(
    ori_rp_SEM = ori_rp_sd / sqrt(n),
    ori_rp_CI = ori_rp_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

#adding levels to subID to be able to order
data_by_pp3$SubID <-
  factor(
    data_by_pp3$SubID,
    levels = participant_list
  )



my_plot3.0 <- ggplot() +
  
  
  
  geom_point(
    data = data_across_pp3,
    aes(x = ori_deg,
        y = ori_rp_mean,
        group = RM,
        color = RM,
        size = n),
    position = position_dodge(0.5),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE) +
  
  
  geom_errorbar(
    data = data_across_pp3,
    aes(
      x = ori_deg,
      y = ori_rp_mean,
      group = RM,
      color = RM,
      ymin = ori_rp_mean - ori_rp_CI,
      ymax = ori_rp_mean + ori_rp_CI,
      
    ),
    
    size  = 1.0,
    width = .00,
    alpha = 0.9,
    position = position_dodge(0.5)
  ) +
  
  facet_wrap( ~ oriented_line_location + hemisphere, nrow = nrow, labeller = labeller(
    oriented_line_location =
      c(
        "inner" = "Inner Line Tilted",
        "middle" = "Middle Line Tilted",
        "outer" = "Outer Line Tilted",
        "middle_inner" = "Middle-Inner Line Tilted",
        "middle_outer" = "Middle-Outer Line Tilted"
      ),
    hemisphere = 
      c(
        "left" = "Left Side of the Fixation",
        "right" = "Right Side of the Fixation"
      ))
  ) +
  
  
  stat_smooth(
    data = data_across_pp3,
    aes(
      x = ori_deg,
      y = ori_rp_mean,
      group = RM,
      color = RM
    ),
    method = "glm",
    size = 2,
    se = FALSE,
    alpha = 0.5,
    geom = "line",
    formula = y ~ poly(x, 2)
    
  ) +
  
  
  scale_color_manual(
    labels = c("non-RM trials", "RM trials"),
    values = c( "#298C8C", "#800074"),
    name = ""
  ) +
  
  theme_my_data  +
  
  
  labs(
    x = "Orientation Degree",
    y = "Orientation Correct Percentage",
    color = "Redundancy Masking"
  )

my_plot3.0
