
GJT_group = read_csv("GJT_group.csv")
GJT_group = GJT_group %>% 
  rename(
    Strategy = Type,
    Novelty = old_new,
    Grammaticality = grammaticality,
    sub = subID
  )

# =========================================================
# GLMM analysis for GJT - grammaticality judgment task
# =========================================================

# Packages
library(lme4)
library(dplyr)

# =========================================================
# 1. Data preparation
# =========================================================

# Make sure variables are factors
GJT_group$sub <- factor(GJT_group$sub)
GJT_group$item  <- factor(GJT_group$item)

GJT_group$Strategy <- factor(GJT_group$Strategy)
GJT_group$Novelty <- factor(GJT_group$Novelty)
GJT_group$Grammaticality <- factor(GJT_group$Grammaticality)

# ---------------------------------------------------------
# Sum contrast coding for 2-level categorical predictors: (-0.5,0.5)
# ---------------------------------------------------------

contrasts(GJT_group$Strategy) <- contr.sum(2) / 2
contrasts(GJT_group$Novelty) <- contr.sum(2) / 2
contrasts(GJT_group$Grammaticality) <- contr.sum(2) / 2

# Check coding
contrasts(GJT_group$Strategy)
contrasts(GJT_group$Novelty)
contrasts(GJT_group$Grammaticality)

# =========================================================
# 2. Null model
# =========================================================

null_model <- glmer(
  Acc ~ 1 +
    (1 | sub) +
    (1 | item),
  
  family = binomial(link = "logit"),
  data = GJT_group,
  
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
  Acc ~ Strategy * Novelty * Grammaticality +
    (1 | sub) +
    (1 | item),
  
  family = binomial(link = "logit"),
  data = GJT_group,
  
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
# 5. Likelihood ratio tests for fixed effects
# =========================================================

# ---------------------------------------------------------
# 5.1 Three-way interaction
# ---------------------------------------------------------

model_no_3way <- glmer(
  Acc ~
    Strategy +
    Novelty +
    Grammaticality +
    
    Strategy:Novelty +
    Strategy:Grammaticality +
    Novelty:Grammaticality +
    
    (1 | sub) +
    (1 | item),
  
  family = binomial,
  data = GJT_group,
  
  # control = glmerControl(
  #   optimizer = "bobyqa",
  #   optCtrl = list(maxfun = 200000)
  # )
)

anova(model_no_3way, full_model, test = "Chisq")

# =========================================================
# 5.2 Strategy × Novelty interaction
# =========================================================

model_no_Strategy_oldnew <- glmer(
  Acc ~
    Strategy +
    Novelty +
    Grammaticality +
    
    Strategy:Grammaticality +
    Novelty:Grammaticality +
    
    (1 | sub) +
    (1 | item),
  
  family = binomial,
  data = GJT_group,
  
  # control = glmerControl(
  #   optimizer = "bobyqa",
  #   optCtrl = list(maxfun = 200000)
  # )
)

anova(model_no_Strategy_oldnew, full_model, test = "Chisq")

# =========================================================
# 5.3 Strategy × Grammaticality interaction
# =========================================================

model_no_Strategy_gram <- glmer(
  Acc ~
    Strategy +
    Novelty +
    Grammaticality +
    
    Strategy:Novelty +
    Novelty:Grammaticality +
    
    (1 | sub) +
    (1 | item),
  
  family = binomial,
  data = GJT_group,
  
  # control = glmerControl(
  #   optimizer = "bobyqa",
  #   optCtrl = list(maxfun = 200000)
  # )
)

anova(model_no_Strategy_gram, full_model, test = "Chisq")

# =========================================================
# 5.4 Novelty × Grammaticality interaction
# =========================================================

model_no_oldnew_gram <- glmer(
  Acc ~
    Strategy +
    Novelty +
    Grammaticality +
    
    Strategy:Novelty +
    Strategy:Grammaticality +
    
    (1 | sub) +
    (1 | item),
  
  family = binomial,
  data = GJT_group,
  
  # control = glmerControl(
  #   optimizer = "bobyqa",
  #   optCtrl = list(maxfun = 200000)
  # )
)

anova(model_no_oldnew_gram, full_model, test = "Chisq")

# =========================================================
# 5.5 Main effect of Strategy
# =========================================================

model_no_Strategy <- glmer(
  Acc ~
    Novelty * Grammaticality +
    
    (1 | sub) +
    (1 | item),
  
  family = binomial,
  data = GJT_group,
  
  # control = glmerControl(
  #   optimizer = "bobyqa",
  #   optCtrl = list(maxfun = 200000)
  # )
)

anova(model_no_Strategy, full_model, test = "Chisq")

# =========================================================
# 5.6 Main effect of Novelty
# =========================================================

model_no_oldnew <- glmer(
  Acc ~
    Strategy * Grammaticality +
    
    (1 | sub) +
    (1 | item),
  
  family = binomial,
  data = GJT_group,
  
  # control = glmerControl(
  #   optimizer = "bobyqa",
  #   optCtrl = list(maxfun = 200000)
  # )
)

anova(model_no_oldnew, full_model, test = "Chisq")

# =========================================================
# 5.7 Main effect of Grammaticality
# =========================================================

model_no_gram <- glmer(
  Acc ~
    Strategy * Novelty +
    
    (1 | sub) +
    (1 | item),
  
  family = binomial,
  data = GJT_group,
  
  # control = glmerControl(
  #   optimizer = "bobyqa",
  #   optCtrl = list(maxfun = 200000)
  # )
)

anova(model_no_gram, full_model, test = "Chisq")

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
        pairwise ~ Strategy | Grammaticality * Novelty)

emmeans(full_model,
        pairwise ~ Strategy | Grammaticality)

emmeans(full_model,
        pairwise ~ Strategy)

emmeans(full_model,
        pairwise ~ Grammaticality)

# # =========================================================
# # 9. Predicted probabilities
# # =========================================================
# 
# emmeans(full_model,
#         ~ Strategy * Novelty * Grammaticality,
#         type = "response")