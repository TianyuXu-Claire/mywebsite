
Recall_group = read_csv("Recall_group.csv")
Recall_group = Recall_group %>% 
  rename(
    Strategy = Group,
    sub = subID
  )

# =========================================================
# GLMM analysis for memory recall task
# =========================================================

# Packages
library(lme4)
library(dplyr)

# =========================================================
# 1. Data preparation
# =========================================================

# Make sure variables are factors
Recall_group$sub <- factor(Recall_group$sub)
Recall_group$item  <- factor(Recall_group$item)

Recall_group$Strategy <- factor(Recall_group$Strategy)

# ---------------------------------------------------------
# Sum contrast coding for 2-level categorical predictors: (-0.5,0.5)
# ---------------------------------------------------------

contrasts(Recall_group$Strategy) <- contr.sum(2) / 2

# Check coding
contrasts(Recall_group$Strategy)

# =========================================================
# 2. Null model
# =========================================================

null_model <- glmer(
  Acc_ls ~ 1 +
    (1 | sub) +
    (1 | item),
  
  family = binomial(link = "logit"),
  data = Recall_group,
  
  # control = glmerControl(
  #   optimizer = "bobyqa",
  #   optCtrl = list(maxfun = 200000)
  # )
)

summary(null_model)

# =========================================================
# 3. Full model
# =========================================================

full_model <- glmer(
  Acc_ls ~ Strategy +
    (1 | sub) +
    (1 | item),
  
  family = binomial(link = "logit"),
  data = Recall_group,
  
  # control = glmerControl(
  #   optimizer = "bobyqa",
  #   optCtrl = list(maxfun = 200000)
  # )
)

summary(full_model)

# =========================================================
# 4. Compare null vs. full model
# =========================================================

anova(null_model, full_model, test = "Chisq")

# =========================================================
# 6. Optional: automatic LRT table
# =========================================================

# drop1(full_model, test = "Chisq")

# =========================================================
# 7. Check convergence / singularity
# =========================================================

# isSingular(full_model)

# =========================================================
# 8. Estimated marginal means (post-hoc)
# =========================================================

library(emmeans)

emmeans(full_model,
        pairwise ~ Strategy)

# # =========================================================
# # 9. Predicted probabilities
# # =========================================================
# 
# emmeans(full_model,
#         ~ Strategy,
#         type = "response")