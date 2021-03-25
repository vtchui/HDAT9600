#Import data
birth<- read.csv("C:\\Users\\61413\\Documents\\HDAT9600\\Week 6\\birth.csv")
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
# none of the variables are siginificant at 5% significance level. This model looks rubbish compared to full model, higher AIC...

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