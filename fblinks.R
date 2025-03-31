### PREPEARING WORKING SPACE
install.packages("ggplot2")
install.packages("writexl")
install.packages("dplyr")
install.packages("Matrix")

library(lme4)
library(performance)
library(ordinal)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(writexl)
library(nlme)

################################## DATA LOADING AND PREPROCESSING
BD <- fblinks_1

colnames(BD)

setRepositories()

ggplot(BD, aes(Type, blinks)) + 
  geom_boxplot()

hist(BD$blinks)
EM3 = EM |> filter(blinks < 1)

### Scale for confirmation bias to news (consistency from 1 to 7)
BD = BD |> 
  mutate(confirmation_news = case_when(news_valence == "positive" ~ attitude, 
                                       news_valence == "negative" ~ 8 - attitude))

### Scale for confirmation bias to commentary (consistency from 1 to 7)
BD = BD |> 
  mutate(confirmation_comment = case_when(comment_valence == "positive" ~ attitude, 
                                          comment_valence == "negative" ~ 8 - attitude))

############# H1 testing #########################

############# Random factors testing
model0 = lmer(credibility ~ 1 + (1 | ID) + (1 | id_news), data = BD) 
icc(model0) 
model1 = lmer(credibility ~ 1 + (1 | ID), data = BD) 
icc(model1)
model2 = lmer(credibility ~ 1 + (1 | id_news), data = BD) 
icc(model2) 

colnames(BD)

Model_A = lmer(credibility ~ 1 
               + scale(age)
               + as.factor(gender)
               + conf_text
               + conf_comm
               + news_issue
               +(1 | id_news), data = BD)

Model_A1 = lmer(credibility ~ 1 
               + scale(age)
               + as.factor(gender)
               + conf_text*conf_comm
               + news_issue
               +(1 | id_news), data = BD)

Model_A2 = lmer(credibility ~ 1 
                + scale(age)
                + as.factor(gender)
                + conf_text*conf_comm*news_issue
                +(1 | id_news), data = BD)

Model_A3 = lmer(credibility ~ 1 
                + scale(age)
                + as.factor(gender)
                + conf_text*news_issue
                + conf_comm
                +(1 | id_news), data = BD)

########## Model_comparing#############33
anova(Model_A, Model_A1, Model_A2, Model_A3)

#npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)
#Model_A    10 5510.7 5564.0 -2745.3   5490.7                      
#Model_A1   12 5514.3 5578.3 -2745.1   5490.3  0.3930  2     0.8216
#Model_A3   12 5512.0 5576.0 -2744.0   5488.0  2.2396  0           
#Model_A2   22 5518.4 5635.7 -2737.2   5474.4 13.6141 10     0.1913

################################ FINAL MODEL #####################
r2(Model_A)
summary(Model_A)
sjPlot::tab_model(Model_A)

############################# PROVIDING F-STATISTICS FOR LMEM ######################

anova_results <- anova(Model_A)
print(anova_results)
normality(Model_A)

############## PAIRWISE COMPARISSON #################
install.packages("emmeans")
library(emmeans) # v. 1.7.0
library(magrittr) # v. 2.0.1
emm1 = emmeans(Model_A, specs = pairwise ~ news_issue)
emm2 = emmeans(Model_A, specs = pairwise ~ conf_comm)
emm3 = emmeans(Model_B, specs = pairwise ~ news_issue)
emm4 = emmeans(Model_B, specs = pairwise ~ Type)

####################### Random factors testing
model_b0 = lmer(blinks ~ 1 + (1 | ID) + (1 | id_news), data = BD) 
icc(model_b0) 
model_b1 = lmer(blinks ~ 1 + (1 | ID), data = BD2) 
icc(model_b1)
model_b2 = lmer(blinks ~ 1 + (1 | id_news), data = BD2) 
icc(model_b2) 

colnames(BD)

Model_B = lmer(blinks ~ 1 
               + scale(age)
               + as.factor(gender)
               + Type
               + news_issue
               +(1 | ID), data = BD)

r2(Model_B)
summary(Model_B)
sjPlot::tab_model(Model_B)

