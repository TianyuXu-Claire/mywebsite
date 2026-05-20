library(tidyverse)
# erp = "GJT_MeanAmp_Diff_350-500.csv"      # N400 effect
erp = "GJT_MeanAmp_Diff_500-900.csv"   # P600 effect
erp = read.csv(erp)
erp = erp %>% 
  rename(
    Strategy = Type,
    sub = subID
  )

# =========================================================
# LMM analysis for ERP data (with roi)
# =========================================================
library(lme4)
library(lmerTest)
library(emmeans)

# =========================================================
# 1. Data preparation (Selection of ROI)
# =========================================================

# roi = c("Cz", "CP1", "CP2", "Pz")   # N400
# roi = c("Cz", "CP1", "CP2", "Pz", "POz")   # P600
#e.g. c("CP1","CP2","CPz","P1","P2","Pz"))
# erp_roi = erp %>%
#   filter(Electrode %in%
#            roi)
erp_roi = erp

# Averaging electrode amplitudes in ROI
erp_roi = erp_roi %>%
  group_by(sub, Strategy, Grammar) %>%
  summarise(
    MeanAmp = mean(MeanAmp),
    .groups = "drop"
  )

# Make sure variables are factors
erp_roi$sub <- factor(erp_roi$sub)
erp_roi$Strategy <- factor(erp_roi$Strategy)
erp_roi$Grammar <- factor(erp_roi$Grammar)
# =========================================================
# 2. Null model
# =========================================================
null_model <- lmer(
  MeanAmp ~ 1 +
    (1 | sub),
  data = erp_roi
)
summary(null_model)

# =========================================================
# 3. Full model
# =========================================================
full_model <- lmer(
  MeanAmp ~ Strategy*Grammar +
    (1 | sub),
  data = erp_roi
)
summary(full_model)

# =========================================================
# 4. Compare null vs. full model
# =========================================================
anova(full_model)
anova(null_model, full_model, test = "Chisq")

# =========================================================
# 5. Likelihood ratio tests for fixed effects
# =========================================================
# ---------------------------------------------------------
# 5.1 Strategy*Grammar interaction
# ---------------------------------------------------------
model_no_interaction <- lmer(
  MeanAmp ~ Strategy + Grammar +
    (1 | sub),
  data = erp_roi
)
anova(model_no_interaction, full_model, test = "Chisq")
# ---------------------------------------------------------
# 5.2 Main effect of Strategy
# ---------------------------------------------------------
model_no_Strategy <- lmer(
  MeanAmp ~ Grammar +
    (1 | sub),
  data = erp_roi
)
anova(model_no_Strategy, full_model, test = "Chisq")
# ---------------------------------------------------------
# 5.3 Main effect of Grammar
# ---------------------------------------------------------
model_no_Grammar <- lmer(
  MeanAmp ~ Strategy +
    (1 | sub),
  data = erp_roi
)
anova(model_no_Grammar, full_model, test = "Chisq")

# =========================================================
# 6. Estimated marginal means (post-hoc)
# =========================================================
emmeans(full_model,pairwise ~ Strategy)
emmeans(full_model,pairwise ~ Grammar)


