library(tidyverse)

# Read all raw files
data_dir = "SENSE_Data_Behavioural"
file_list = list.files(path = data_dir, pattern = "\\.csv$", full.names = TRUE)

# Sort files into different phases/tasks: 0. Vocabulary test    1. Learning Phase     2. GJT      3. Recall
voc_files = file_list[str_detect(file_list, "vocabulary test")]
learning_files = file_list[str_detect(file_list, "Learning")]
gjt_files = file_list[str_detect(file_list, "GJT")]
recall_files = file_list[str_detect(file_list, "Recall")]

# For each task, read each file from each subject (participant) and filter for columns/rows needed; then rename some columns; then write the csv. file
### 0. Vocabulary test ####
voc_files = map_dfr(voc_files, function(subfile){
  data = read.csv(subfile)
  data = data %>%
    filter(!is.na(thisN))                 # filter out blank rows
  sub_id = str_extract(basename(subfile), "S\\d+")  # extract subject ID (e.g. subtract "S001" from the original file name "S001_xxx...")
  
  data_selected = data %>%
    select(thisN, Category, mouse_resp.corr)
  
  data_selected = data_selected %>%
    mutate(subID = sub_id)
  
  print(sub_id)
  return(data_selected)
})

voc_files = voc_files %>%
  rename(
    nTrial = thisN,
    Acc = mouse_resp.corr
  )

write_csv(voc_files, "Vocabulary test.csv")

### 1. Learning ####
learning_files = map_dfr(learning_files, function(subfile){
  data = read_csv(subfile)
  data = data %>%
    filter(!is.na(Acc), !is.na(picture), !picture %in% c('A.png', 'B.png'))                  # filter for the critical learning trials only (i.e. restudy/retrieval practice)
  sub_id = str_extract(basename(subfile), "S\\d+")  
  
  data_selected = data %>%
    select(List, fullSentenceEN, sentenceType, sentenceType2, studyType, targetPoS, Acc)
  
  data_selected = data_selected %>%
    mutate(subID = sub_id,
           nTrial = (0:191) # 192 trials for critical learning
    )
  
  print(sub_id)
  return(data_selected)
})

write_csv(learning_files, "Learning_critical.csv")

### 2. GJT ####
gjt_files = map_dfr(gjt_files, function(subfile){
  data = read_csv(subfile)
  data = data %>%
    filter(!is.na(trials_9.thisN))                  # filter out the practice trials and blank rows
  sub_id = str_extract(basename(subfile), "S\\d+") 
  
  data_selected = data %>%
    select(trials_9.thisN, Row, sentenceType, sentenceType2, old_new, grammaticality, key_resp_3.keys, key_resp_3.corr, key_resp_3.rt)
  
  data_selected = data_selected %>%
    mutate(subID = sub_id)
  
  return(data_selected)
})

gjt_files = gjt_files %>%
  rename(
    nTrial = trials_9.thisN,
    item = Row,
    Key = key_resp_3.keys,
    Acc = key_resp_3.corr,
    RT = key_resp_3.rt
  )

write_csv(gjt_files, "GJT.csv")

### 3. Recall ####
# Load the relevnat files, and select colomuns needed
recall_files = map_dfr(recall_files, function(subfile){
  data = read_csv(subfile)
  data = data %>%
    filter(!is.na(thisN))                  # filter out blank rows
  sub_id = str_extract(basename(subfile), "S\\d+") 
  
  data_selected = data %>%
    select(thisN, row, List, fullSentenceEN, sentenceType, sentenceType2, Group, Word1_PoS, Word2_PoS, Word1_RP_Item, Word2_RP_Item, target_full, textbox.text, Acc)
  
  data_selected = data_selected %>%
    mutate(subID = sub_id)
  
  return(data_selected)
})

# rename some of the columns for clarity
recall_files = recall_files %>%
  rename(
    nTrial = thisN,
    item = row,
    Target = target_full,
    Ans = textbox.text,
  )

# Check and write down the accuracy of two blanks separately: "Acc_ls" (ls = lower standard) = 1 when at least one word is answered correctly
recall_files = recall_files %>% 
  separate(Target,
           into = c("Target1", "Target2"),
           sep = " ",
           remove = FALSE) %>% 
    separate(Ans,
             into = c("Ans1", "Ans2"),
             sep = " ",
             remove = FALSE) %>% 
    mutate(
      Acc_word1 = case_when(
        Target1 == Ans1 ~ 1,
        is.na(Ans1) ~ 0,
        TRUE ~ 0
        ),
      Acc_word2 = case_when(
        Target2 == Ans2 ~ 1,
        is.na(Ans2) ~ 0,
        TRUE ~ 0
      ),
      Acc_ls = if_else(
        Acc_word1 == 1 | Acc_word2 == 1,
        1,
        0
      )
    )

write_csv(recall_files, "Recall.csv")

