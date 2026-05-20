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
# roi = c("CP1","CP2","CPz","Cz","P1","P2","Pz","PO3","PO4","POz")
# roi = c("CP1","CP2","Cz", "Pz","POz")
# erp_roi = erp %>%
#   filter(Electrode %in% roi)
erp_roi = erp

# Averaging electrode amplitudes in ROI
erp_roi = erp_roi %>%
  group_by(sub, Strategy) %>%
  summarise(
    MeanAmp = mean(MeanAmp),
    .groups = "drop"
  )

# Make sure variables are factors
erp_roi$sub <- factor(erp_roi$sub)
erp_roi$Strategy <- factor(erp_roi$Strategy)

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
  MeanAmp ~ Strategy +
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
# 5. Estimated marginal means (post-hoc)
# =========================================================
emmeans(full_model,pairwise ~ Strategy)

