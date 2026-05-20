library(tidyverse)
library(lme4)
library(brms)

### 0. Vocabulary test ####
  voc_data = read.csv("Vocabulary test.csv")

  ## change accuracy to integer format
    voc_data$Acc = gsub("\\[|\\]", "", voc_data$Acc)
    voc_data$Acc = as.integer(voc_data$Acc)
    str(voc_data)
  
  ## descriptives
    # extract mean accuracy for each subject
    voc_sub_mean = voc_data %>%
      group_by(subID) %>%
      summarise(
        sub_acc_mean = mean(Acc, na.rm = TRUE)
      )
    # compute group mean
    mean(voc_sub_mean$sub_acc_mean)
    sd(voc_sub_mean$sub_acc_mean)
    range(voc_sub_mean$sub_acc_mean)

### 1. Learning (critical learning only: retrieval practice (RP) vs. restudy (S)) ####
  learn_data = read_csv("Learning_critical.csv")
  str(learn_data)
  ## Conditions:
    # 1. Learning strategy          "List"          A = Classifier - RP   /B = Coverb RP 
    # 2. Sentence study type        "studyType"     "retrieve"            /"restudy"           

  ## descriptives at first glance
    summary(learn_data)
  ## descriptives grouped by Study type   (Category: 1 = verb, 0 = noun)
    # extract mean accuracy for each subject
      learn_sub_mean = learn_data %>%
        group_by(subID, studyType) %>%
        summarise(
          sub_acc_mean = mean(Acc, na.rm = TRUE)
          )
    # compute group mean
      mean(learn_sub_mean$sub_acc_mean[learn_sub_mean$studyType == "retrieve"])
      sd(learn_sub_mean$sub_acc_mean[learn_sub_mean$studyType == "retrieve"])
      range(learn_sub_mean$sub_acc_mean[learn_sub_mean$studyType == "retrieve"])
      mean(learn_sub_mean$sub_acc_mean[learn_sub_mean$studyType == "restudy"])
      sd(learn_sub_mean$sub_acc_mean[learn_sub_mean$studyType == "restudy"])
      range(learn_sub_mean$sub_acc_mean[learn_sub_mean$studyType == "restudy"])
      # see restudy & retrieval
    # (important for GJT later) extract learning strategy information (counterbalanced by "List")
      learn_strategy_info = learn_data %>% 
        group_by (subID, List) %>% 
        summarise(
          List = first(List),
          subID = first(subID)
        )
      write.csv(learn_strategy_info, "learn_strategy_info.csv")

### 2. GJT (Grammaticality Judgement Task)  ####
  GJT_data = read.csv("GJT.csv")
  str(GJT_data)
  ## Conditions:
    # 1. Learning strategy            "List"              A = Classifier - RP / B = Coverb RP 
    # 2. Sentence structure           "sentenceType2"     "coverb" / "classifier" (-> decide RP or S?)
    # 3. Sentence novelty(transfer)   "old_new"           "old" / "new"
    # 4. Sentence grammaticality      "grammaticality"    "Grammatical" / "Ungrammatical"
  
  ## Mapping the "List" info to each subject (using previously extracted "learn_strategy_info" data)
    GJT_data = GJT_data %>% 
      left_join(learn_strategy_info, by = "subID")
  
  ## Collapsing the counterbalanced factor Learning strategy ("List") with Sentence structure ("sentenceType2") to a new column named "Type"
    GJT_data = GJT_data %>% 
      mutate(
        Type = case_when(
          List == "A" & sentenceType2 == "classifier" ~ "retrieval practice",
          List == "A" & sentenceType2 == "coverb"     ~ "restudy",
          List == "B" & sentenceType2 == "coverb"     ~ "retrieval practice",
          List == "B" & sentenceType2 == "classifier" ~ "restudy",
          TRUE ~ NA_character_
        )
      )
  ## Adding a new column "Endorsement" (for later computation of 'Endorsement Rate' = mean proportion of "grammatical" responses)
    # "Endorsement": 1 = key response grammatical ('j')   0 = key response ungrammatical ('k')
    GJT_data = GJT_data %>% 
      mutate(
        Endorsement = case_when(
          Key == "j" ~ 1L,
          Key == "k" ~ 0L,
          TRUE ~ NA_integer_
        )
      )
    # write the file for GLMM
    write_csv(GJT_data, "GJT_group.csv")
  
  ## descriptives at first glance
    summary(GJT_data)
  
  ## descriptives grouped by Conditions 2, 3, 4
    # extract mean accuracy for each subject
    GJT_sub_mean = GJT_data %>%
      group_by(subID, List, Type, sentenceType2, old_new, grammaticality) %>%
      summarise(
        sub_acc_mean = mean(Acc, na.rm = TRUE),
        sub_rt_mean = mean(RT, na.rm = TRUE),
        sub_endo_mean = mean(Endorsement, na.rm = TRUE)
      )
    # t test to see if the accuracy is above chance level
    t.test(GJT_sub_mean$sub_acc_mean, mu = 0.5)
    
    # write the file for plotting
    write_csv(GJT_sub_mean, "GJT_sub.csv")
    
    # compute group descriptives
    mean(GJT_sub_mean$sub_acc_mean)
    sd = sd(GJT_sub_mean$sub_acc_mean)
    range(GJT_sub_mean$sub_acc_mean)
    
    # save to df: grouping by List, Type, sentenceType2, old_new, grammaticality
    GJT_group_mean = GJT_sub_mean %>%
      group_by(List, Type, sentenceType2, old_new, grammaticality) %>%
      summarise(
        ACC_mean = mean(sub_acc_mean),
        ACC_sd = sd(sub_acc_mean),
        RT_mean = mean(sub_rt_mean),
        RT_sd = sd(sub_acc_mean),
        ENDORSE_mean = mean(sub_endo_mean),
        ENDORSE_sd = sd(sub_endo_mean),
        n = n(),
        ACC_sem = ACC_sd / sqrt(n),
        RT_sem = RT_sd / sqrt(n),
        ENDORSE_sem = ENDORSE_sd / sqrt(n)
      )

### 3. Recall ####
  Recall_data = read.csv("Recall.csv")
  str(Recall_data)
  
  # change some names
  Recall_data = Recall_data %>% 
    mutate(
      Group = case_when(
        Group == "Retrieval" ~ "retrieval practice",
        Group == "Restudy" ~ "restudy"
      )
    )
  ## descriptives at first glance
    summary(Recall_data)
  ## descriptives grouped by learning strategy ('Group')
    # extract mean accuracy for each subject: either Acc_ls, or Acc!!!
    Recall_sub_mean = Recall_data %>%
      group_by(subID, Group) %>%
      summarise(
        sub_acc_mean = mean(Acc_ls, na.rm = TRUE)
      )
    
    # compute group descriptives
    mean(Recall_sub_mean$sub_acc_mean)
    sd(Recall_sub_mean$sub_acc_mean)
    range(Recall_sub_mean$sub_acc_mean)
    mean(Recall_sub_mean$sub_acc_mean[Recall_sub_mean$Group == "restudy"])
    sd(Recall_sub_mean$sub_acc_mean[Recall_sub_mean$Group == "restudy"])
    range(Recall_sub_mean$sub_acc_mean[Recall_sub_mean$Group == "restudy"])
    mean(Recall_sub_mean$sub_acc_mean[Recall_sub_mean$Group == "retrieval practice"])
    sd(Recall_sub_mean$sub_acc_mean[Recall_sub_mean$Group == "retrieval practice"])
    range(Recall_sub_mean$sub_acc_mean[Recall_sub_mean$Group == "retrieval practice"])
    
    
    # compute group mean by learning strategy
    Recall_sub_mean %>%
      group_by(Group) %>%
      summarise(
        ACC_mean = mean(sub_acc_mean),
        ACC_sd = sd(sub_acc_mean),
        n = n(),
        sem = ACC_sd / sqrt(n)
      )
    
    # write the file for plotting
    write_csv(Recall_sub_mean, "Recall_sub.csv")
    # write the file for GLMM
    write_csv(Recall_data, "Recall_group.csv")
    
