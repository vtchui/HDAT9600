# Data dictionary (source: https://rdrr.io/cran/faraway/man/prostate.html)
#   lcavol    log(cancer volume)
#   lweight   log(prostate weight)
#   age       age
#   lbph      log(benign prostatic hyperplasia amount)
#   svi       seminal vesicle invasion [0=no, 1=yes]
#   lcp       log(capsular penetration)
#   gleason   Gleason score
#   pgg45     percentage Gleason scores 4 or 5
#   lpsa      log(prostate specific antigen)

# load dataset
data(prostate, package="faraway")
# load required packages
library(tidyverse)

## simple EDA ##
# Examine dataset
dim(prostate)
str(prostate)
head(prostate)
summary(prostate)

# find minimum values for all variables
mins <- rep(0,9)
for(i in 1:9){
  mins[i] <- min(prostate[,i])
}
mins

# find maximum values for all variables
maxs <- rep(0,9)
for(i in 1:9){
  maxs[i] <- max(prostate[,i])
}
maxs

# output min and max into readable format
min_max <- data.frame(variable=colnames(prostate), min=mins, max=maxs)
print(min_max)

# examine missingness
table(is.na(prostate)) 

# graph of log(cancer volume)
ggplot(data = prostate) +
  geom_freqpoly(mapping = aes(x=lcavol), binwidth=1)
# graph of log(prostate weight)
ggplot(data = prostate) +
  geom_freqpoly(mapping = aes(x=lweight), binwidth=1)
# graph of age
ggplot(data = prostate) +
  geom_freqpoly(mapping = aes(x=age), binwidth=5)
# graph of log(bph amount)
ggplot(data = prostate) +
  geom_freqpoly(mapping = aes(x=lbph), binwidth=0.25)
# table of seminal vesicle invasion (dichotomous)
prostate %>% count(svi)
# graph of log(capsular penetration)
ggplot(data = prostate) +
  geom_freqpoly(mapping = aes(x=lcp), binwidth=0.25)
# graph of gleason score
ggplot(data = prostate) +
  geom_freqpoly(mapping = aes(x=gleason), binwidth=1)
# graph of percentage gleason score 4 or 5
ggplot(data = prostate) +
  geom_freqpoly(mapping = aes(x=pgg45), binwidth=10)
# graph of log(psa)
ggplot(data = prostate) +
  geom_freqpoly(mapping = aes(x=lpsa), binwidth=0.5)

## create models ##
# linear model 1 
# included predictors: lcavol
lmod1 <- lm(lpsa ~ lcavol, data=prostate)
lmod1_summ <- summary(lmod1)
lmod1_rsq <- lmod1_summ$adj.r.squared
lmod1_RSE <- lmod1_summ$sigma

# display first model r squared and RSE
lmod1_outputs <- data.frame(model="lmod1", Rsquared=lmod1_rsq, RSE=lmod1_RSE)
print(lmod1_outputs)

# linear model 2
# included predictors: lcavol, lweight
lmod2 <- lm(lpsa ~ lcavol + lweight, data=prostate)
lmod2_summ <- summary(lmod2)
lmod2_rsq <- lmod2_summ$adj.r.squared
lmod2_RSE <- lmod2_summ$sigma

# linear model 3
# included predictors: lcavol, lweight, svi
lmod3 <- lm(lpsa ~ lcavol + lweight + svi, data=prostate)
lmod3_summ <- summary(lmod3)
lmod3_rsq <- lmod3_summ$adj.r.squared
lmod3_RSE <- lmod3_summ$sigma

# linear model 4
# included predictors: lcavol, lweight, svi, lbph
lmod4 <- lm(lpsa ~ lcavol + lweight + svi + lbph, data=prostate)
lmod4_summ <- summary(lmod4)
lmod4_rsq <- lmod4_summ$adj.r.squared
lmod4_RSE <- lmod4_summ$sigma

# linear model 5
# included predictors: lcavol, lweight, svi, lbph, age
lmod5 <- lm(lpsa ~ lcavol + lweight + svi + lbph + age, data=prostate)
lmod5_summ <- summary(lmod5)
lmod5_rsq <- lmod5_summ$adj.r.squared
lmod5_RSE <- lmod5_summ$sigma

# linear model 6
# included predictors: lcavol, lweight, svi, lbph, age, lcp
lmod6 <- lm(lpsa ~ lcavol + lweight + svi + lbph + age + lcp, data=prostate)
lmod6_summ <- summary(lmod6)
lmod6_rsq <- lmod6_summ$adj.r.squared
lmod6_RSE <- lmod6_summ$sigma

# linear model 7
# included predictors: lcavol, lweight, svi, lbph, age, lcp, pgg45
lmod7 <- lm(lpsa ~ lcavol + lweight + svi + lbph + age + lcp + pgg45, data=prostate)
lmod7_summ <- summary(lmod7)
lmod7_rsq <- lmod7_summ$adj.r.squared
lmod7_RSE <- lmod7_summ$sigma

# linear model 8
# included predictors: lcavol, lweight, svi, lbph, age, lcp, pgg45, gleason (full model)
lmod8 <- lm(lpsa ~ lcavol + lweight + svi + lbph + age + lcp + pgg45 + gleason, data=prostate)
lmod8_summ <- summary(lmod8)
lmod8_rsq <- lmod8_summ$adj.r.squared
lmod8_RSE <- lmod8_summ$sigma

# display r squared and RSE for all models
lmod_all_outputs <- data.frame(model=c("lmod1","lmod2","lmod3","lmod4","lmod5","lmod6","lmod7","lmod8"),
                               Rsquared=c(lmod1_rsq, lmod2_rsq, lmod3_rsq, lmod4_rsq, lmod5_rsq, lmod6_rsq, lmod7_rsq, lmod8_rsq),
                               RSE=c(lmod1_RSE, lmod2_RSE, lmod3_RSE, lmod4_RSE, lmod5_RSE, lmod6_RSE, lmod7_RSE, lmod8_RSE))
print(lmod_all_outputs)

# plot r squared as predictors are added to the model
ggplot(data = lmod_all_outputs) +
  geom_point(mapping = aes(x = model, y = Rsquared))

# plot RSE as predictors are added to the model
ggplot(data = lmod_all_outputs) +
  geom_point(mapping = aes(x = model, y = RSE))
