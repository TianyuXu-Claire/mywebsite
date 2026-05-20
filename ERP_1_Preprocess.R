library(tidyverse)
erp_filename = "SENSE_GJT_Mean_Amplitude_Diff_Waves_350-500.txt"
# erp_filename = "SENSE_GJT_Mean_Amplitude_Diff_Waves_500-900.txt"
erp_output = "GJT_MeanAmp_Diff_350-500.csv"
# erp_output = "GJT_MeanAmp_Diff_500-900.csv"

# Reading the file and finding subject ID
erp = read.delim(erp_filename)
erp = erp %>%
  mutate(
    subID = str_extract(ERPset, "S\\d+")
  )

# Converting the file into long formats
erp_long = erp %>%
  pivot_longer(
    cols = -c(ERPset, subID),
    names_to = "Channel",
    values_to = "MeanAmp"
  )

# Separating the conditions
erp_long <- erp_long %>%
  separate(
    Channel,
    into = c("Bin", "Grammar", "Word", "Wave", "Electrode"),
    sep = "_"
  )

# Mapping the "List" info to each subject (using previously extracted "learn_strategy_info" data)
learn_strategy_info = read.csv("learn_strategy_info.csv")
erp_long  = erp_long  %>% 
  left_join(learn_strategy_info, by = "subID")

# Collapsing the counterbalanced factor Learning strategy ("List") with Sentence structure ("sentenceType2") to a new column named "Type"
erp_long = erp_long %>% 
  mutate(
    Type = case_when(
      List == "A" & Grammar == "Class" ~ "retrieval practice",
      List == "A" & Grammar == "Coverb"~ "restudy",
      List == "B" & Grammar == "Coverb"~ "retrieval practice",
      List == "B" & Grammar == "Class" ~ "restudy",
      TRUE ~ NA_character_
    )
  )

# write the prepared file for running GLMM
write.csv(erp_long, erp_output)
