# Gerekli kütüphaneleri yükleyin
library(readr)
library(dplyr)
library(openxlsx)


#data path
dosya_yolu <- "D:/Projects/RM_Tilt/raw_data"

#take each datafile path based on the first three numbers
dosya_listesi <- list.files(dosya_yolu, pattern = "\\d{3}", full.names = TRUE)

#create an list to save each participant's dataframes
dataframes <- list()
dataframes_control <- list()

# upload each data file and remove practice trials
for (dosya in dosya_listesi) {
  df <- read_csv(dosya)
  df <- df[-(1:11), ]
  
  #create an column for subject code
  isim <- substr(basename(dosya), 1, 3)
  dataframes[[isim]] <- df
}

#CONTROL EXPERIMENT #####
for (j in seq_along(dataframes)) {
  df_control <- dataframes[[j]]
  
  df_control <- df_control %>%
    dplyr::select(key_resp.keys, amount) %>%
    filter(row_number() > 600) %>%
    filter(!is.na(amount)) %>%
    mutate(response_control = as.integer(gsub("\\D", "", key_resp.keys))) %>%
    mutate(correct = case_when(
      response_control == amount ~ 1, # 1 indicates true
      response_control != amount ~ 0, # 0 indicates error
    ))
  dataframes_control[[j]] <- df_control
}

#combine all control data
birlesik_df_control <- bind_rows(dataframes_control, .id = "Isim")
names(birlesik_df_control)[1] <- "SubID"

birlesik_df_control_sum <- birlesik_df_control %>% 
  group_by(SubID) %>%
  mutate(sum_correct = sum(correct),
         correct_percent = sum_correct/32,
         SubID = as.integer(SubID)) %>%
  ungroup() %>%
  summarize(SubID, correct_percent,
            Mean = mean(correct_percent),
            SD = sd(correct_percent)) %>%
  unique() #PARTICIPANT 5 VERY BAD PERFORMANCE %60 AND participant 4-17 %75 CORRECT
#save the data file as xlsx
write.xlsx(birlesik_df_control_sum, "df_tilt_control.xlsx")


#CLEAN DATA ####
#clean the data for each participant
for (i in seq_along(dataframes)) {
  df <- dataframes[[i]]
  
  df <- df %>%
    dplyr::select(1, 
           4:12,
           orientation_1,
           orientation_2,
           orientation_3,
           orientation_4,
           50,
           51,
           rotation_response.keys,
           response_number,
           task_order) %>%
    filter(row_number() <= 600) %>%
    filter(!is.na(amount)) %>%
    mutate(number_deviation = response_number - amount) %>%
    mutate(RM = case_when(
      number_deviation < 0 ~ "RM",
      number_deviation > 0 ~ "Overestimate",
      number_deviation == 0 ~ "Correct"
    )) %>%
    mutate(rotation_response.keys = case_when(
      rotation_response.keys == 'a' ~ 'left',
      rotation_response.keys == 'd' ~ 'right',
      TRUE ~ rotation_response.keys
    )) %>%
    mutate(direction = case_when(
      orientation_1 + orientation_2 + orientation_3 + orientation_4 < 0 ~ "left",
      TRUE ~ "right"
    )) %>%
    mutate(congruency = case_when(
      direction == hemisphere ~ "congruent",
      TRUE ~ "incongruent"
    )) %>%
    mutate(ori_correct = case_when(
      rotation_response.keys == direction ~ "1",
      TRUE ~ "0"
    )) %>%
    mutate(ori_deg = (orientation_1 + orientation_2 + orientation_3 + orientation_4))
           
  dataframes[[i]] <- df
}


#combine all
birlesik_df <- bind_rows(dataframes, .id = "Isim")
names(birlesik_df)[1] <- "SubID"
colnames(birlesik_df)[colnames(birlesik_df) == "amount"] <- "correct_num"


#save the data file as xlsx
write.xlsx(birlesik_df, "df_tilt.xlsx")

