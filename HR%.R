### Required packages
library(readxl) ## To load excel sheet
library(dplyr) # Data grammar and manipulation
library(rstatix) # Shapiro Wilk and effect size
library(psych) #descriptives
library(kableExtra) #tables
library(lme4) #linear mixed effects models (LMM)
library(lmerTest) #anova like output for LMM
library(ggplot2) #data visualization
library(ggpubr)#data visualization
library(ggprism)##makes plots look like graphad
library(table1) #for descriptives


Df <- read_excel("~/Percentages.xlsx", sheet = "HR%")

View(Df)

## Convert from character to factor data
Df$Sex <- as.factor(Df$Sex)
Df$Condition <- as.factor(Df$Condition)

## Order conditions
Df$Condition <- ordered(Df$Condition,
                        levels = c("Baseline", "Low",
                                   "Moderate","High"))

###### Linear Mixed models VO2
lmModel5 = lmer(HR_PERC ~ Condition + Sex + (1|ID),
                data=Df, REML=FALSE)
summary(lmModel5)

# mixed model
anova(lmModel5)
#test of the random effects in the model
rand(lmModel5)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc5 <- Df %>%
  pairwise_t_test(HR_PERC ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")
pwc5%>%
  kbl(caption = "Pairwise comparison") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df %>% cohens_d(HR_PERC ~ Condition,
                paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")
