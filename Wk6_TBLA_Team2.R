# Low birth weight (defined as weighing less than 2500 grams) is an outcome that has been of concern to physicians for years. 
# This is due to the fact that infant mortality rates and birth defect rates are very high for low birth weight babies. 
# A woman's behaviour during pregnancy (including diet, smoking habits, and receiving prenatal care) 
# can greatly alter the chances of carrying the baby to term and, consequently, of delivering a baby of normal birth weight.

# Consider the data set birth.csv collected at the Baystate Medical Center, Springfield, USA during 1986. 
# Data were collected on 189 women, 59 of which had low birth weight babies and 130 of which had normal birth weight babies. 
# The variables identified in the dataset have been shown to be associated with low birth weight in the obstetrical literature.
# 
# The variables are:
# .low: indicator of birth weight less than 2.5kg (1=less than 2.5 kg, 0=2.5 kg or above)
# .age: mother's age in years
# .lwt: mother's weight in pounds at last menstrual period
# .race: mother's race (1=white, 2=black, 3=other)<br>
# .smoke: smoking status during pregnancy (0=did not smoke, 1=smoked)
# .ptl: had previous premature labours (1=had, 0=had not)
# .ht: history of hypertension (1=history, 0=non history)
# .ui: presence of uterine irritability (1=present, 0=absent)
# .ftv: number of physician visits during the first trimester (0=0, 1=1, 2=2 or more)
# 
# Four variables which were thought to be of importance were:
# .age
# .weight of the subject at her last menstrual period
# .race
# .the number of physician visits during the first trimester of pregnancy.


# Getting the path of your current open file
# Extra code to ensure this file imports birth.csv in local directory for everyone
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))


# Import data
birth <- read.csv("./birth.csv", sep=",", header=TRUE)

head(birth)  





########################################################################################################################

##EDA

summary(birth)

plot(birth[,c("low", "age", "lwt","race","ftv")])

#Histograms
library(ggplot2)

ggplot(data=birth, aes(x=age)) + geom_histogram() + 
  facet_grid(low ~ ., scales = "free") + 
  labs(x="Age", y="Observations")

ggplot(data=birth, aes(x=lwt)) + geom_histogram() + 
  facet_grid(low ~ ., scales = "free") + 
  labs(x="Weight", y="Observations")

ggplot(data=birth, aes(x=race)) + geom_histogram() + 
  facet_grid(low ~ ., scales = "free") + 
  labs(x="Race", y="Observations")

ggplot(data=birth, aes(x=smoke)) + geom_histogram() + 
  facet_grid(low ~ ., scales = "free") + 
  labs(x="", y="Observations")

ggplot(data=birth, aes(x=ftv)) + geom_histogram(binwidth=1) + 
  facet_grid(low ~ ., scales = "free") + 
  labs(x="", y="Observations")

ggplot(data=birth, aes(x=ptl)) + geom_histogram(binwidth=1) + 
  facet_grid(low ~ ., scales = "free") + 
  labs(x="", y="Observations")

ggplot(data=birth, aes(x=ht)) + geom_histogram(binwidth=1) + 
  facet_grid(low ~ ., scales = "free") + 
  labs(x="", y="Observations")

ggplot(data=birth, aes(x=ui)) + geom_histogram(binwidth=1) + 
  facet_grid(low ~ ., scales = "free") + 
  labs(x="", y="Observations")


# For the binary variables
matrix<-birth[,c("low", "smoke","ptl","ht","ui")]
rotate <- function(x) t(apply(x, 2, rev))
image(1:ncol(matrix), 1:nrow(matrix), rotate(as.matrix(matrix)), axes=FALSE, ylab="Observations (red is 1, yellow is 0", xlab="")
# add axis labels, note: 3 <- above, 2 <- left
image(1:ncol(matrix), 1:nrow(matrix), rotate(as.matrix(matrix)), axes=FALSE, add=TRUE)
axis(3, at=1:ncol(matrix), labels=colnames(matrix))

# For the categorical variables
matrix2<-birth[,c("low", "race","ftv")]
image(1:ncol(matrix2), 1:nrow(matrix2), rotate(as.matrix(matrix2)), axes=FALSE, ylab="Observations (red is 1, yellow is 0", xlab="")
# add axis labels, note: 3 <- above, 2 <- left
image(1:ncol(matrix2), 1:nrow(matrix2), rotate(as.matrix(matrix2)), axes=FALSE, add=TRUE)
axis(3, at=1:ncol(matrix2), labels=colnames(matrix2))

#For continuous variables
ggplot(data=birth, aes(x=age, y=lwt, fill)) + 
  geom_point(alpha=0.2, position=position_jitter()) + 
  facet_grid(. ~ low) 

ggplot(data=birth, aes(x=race, y=ftv, fill)) + 
  geom_point(alpha=0.2, position=position_jitter()) + 
  facet_grid(. ~ low) 

########################################################################################################################

##Select Model

#Fit full model

Fullmodel <- glm(low ~ age + lwt + race + smoke + ptl + ht +ui +ftv, family = binomial, data=birth)
summary(Fullmodel)
# ptl, ht, smoke - most significant

#Reduce model based on prior  knowledge
Reducedmodel <- glm(low ~ age +lwt + race + ftv, family = binomial, data=birth)
summary(Reducedmodel)
# none of the variables are significant at 5% significance level. This model looks rubbish compared to full model, higher AIC...

# search the model space for a reduced model using the step() function
logmod_reduced <- step(Fullmodel, trace=1)

#4 iterations, AIC reduced by removing age and ftv.

########################################################################################################################


## Goodness of Fit

birth_nm <- na.omit(birth)

########################################################################################################################

##ROC

library(dplyr)
Fullmodel <- glm(low ~ age + lwt + race + smoke + ptl + ht +ui +ftv, family = binomial, data=birth)

# add the predicted probabilities to the data frame
birth_nm %>% mutate(predprob=predict(Fullmodel, type="response")) -> birth_nm


thresholds <- seq(0.03, 0.5, by=0.01) #no's aren't generated for threshold under 0.03
sensitivities <- numeric(length(thresholds))
specificities <- numeric(length(thresholds))

for (i in seq_along(thresholds)) {
  pp <- ifelse(birth_nm$predprob < thresholds[i], "no", "yes")
  xx <- xtabs( ~ low + pp, data=birth_nm)
  specificities[i] <- xx[1,1] / (xx[1,1] + xx[1,2])
  sensitivities[i] <- xx[2,2] / (xx[2,1] + xx[2,2])
}

# plot the sensitivities and specificities
matplot(thresholds, cbind(sensitivities, specificities), type="ll", lty=1:2,
        xlab="Threshold", ylab="Proportion")

# plot the ROC
plot(1 - specificities, sensitivities, type="l",
     xlab="1 - Specificity", ylab="Sensitivity")
abline(0,1, lty=2)

# Area under curve
library(DescTools)

# sorts sensitivity and specificities into x and y coordinates
x <- sort(1-specificities)
y <- sort(sensitivities)

print(AUC(x,y))
