
# =========================================================
# Plotting for GJT
# =========================================================
GJT_sub = read_csv("GJT_sub.csv")

library(ggbeeswarm)
ggplot(GJT_sub,
       aes(x = Type,
           y = sub_acc_mean,
           color = grammaticality)) +
  
  geom_quasirandom(
    dodge.width = .3,
    alpha = .5,
    size = 2
  ) +
  
  stat_summary(aes(group = grammaticality),
               fun = mean,
               geom = "line",
               linewidth = 1.2,
               position = position_dodge(.3)) +
  
  stat_summary(aes(group = grammaticality),
               fun = mean,
               geom = "point",
               size = 3,
               position = position_dodge(.3)) +
  
  stat_summary(aes(group = grammaticality),
               fun.data = mean_se,
               geom = "errorbar",
               width = .1,
               position = position_dodge(.3)) +
  
  facet_wrap(~ old_new) +
  
  ylim(0,1) +
  
  labs(
    x = "Learning strategy",
    y = "Mean accuracy rates",
    color = "Sentence grammaticality"
  ) +
  
  theme_classic(base_size = 14)


# =========================================================
# Plotting for Memory recall task
# =========================================================
Recall_sub = read.csv("Recall_sub.csv")
library(ggbeeswarm)
ggplot(Recall_sub,
       aes(x = Group,
           y = sub_acc_mean)) +
  
  geom_quasirandom(
    alpha = .5,
    size = 2
  ) +
  
  stat_summary(fun = mean,
               geom = "line",
               linewidth = 1.2,) +
  
  stat_summary(fun = mean,
               geom = "point",
               size = 3,) +
  
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = .1,) +
  
  ylim(0,1) +
  
  labs(
    x = "Learning strategy",
    y = "Mean accuracy rates (lower standard)"
  ) +
  
  theme_classic(base_size = 14)

