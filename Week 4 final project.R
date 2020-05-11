#Week 4 course project
#Class: Regression Models
#Adam Ficke

#Question 1: “Is an automatic or manual transmission better for MPG”
#Question 2: "Quantify the MPG difference between automatic and manual transmissions"

#require packages
require(dplyr)
require(tidyverse)
require(ggplot2)
require(corrplot)
require(RColorBrewer)
require(PerformanceAnalytics)
require(GGally)
require(caret)
require(AppliedPredictiveModeling)
require(glmnet)
require(bestglm)
require(doParallel)

#read in the data, get information about dataset
data("mtcars")
str(mtcars)
? mtcars

#change class
mtcars.fac <-
  mtcars %>% mutate_at(c("cyl", "vs", "am", "gear", "carb"), as.factor)

#examine response data
P1 <- ggplot(data = mtcars.fac,
             aes(x = am, y = mpg)) +
  geom_boxplot()
P1

#exammine distribution of mpg data


#Examine correlation
Cars.Cor <- cor(mtcars)
corrplot(
  Cars.Cor,
  type = "upper",
  order = "hclust",
  col = brewer.pal(n = 5, name = "RdYlBu")
)

#build glm, with Gamma distribution
#gamma based on distribution of mpg data
mdl1 <- glm(mpg ~ ., family = Gamma, data = mtcars.fac)
summary(mdl1)
anova(mdl1, test = "Chisq")

#variable reduction using resampling

cl <- makePSOCKcluster(16)
registerDoParallel(cl)
set.seed(100)

subsets <- c(1:11)

ctrl <- rfeControl(
  functions = rfFuncs,
  method = "repeatedcv",
  repeats = 10,
  verbose = FALSE
)

lmProfile <- rfe(
  x = mtcars.fac[, 2:11],
  y = mtcars.fac$mpg,
  sizes = subsets,
  rfeControl = ctrl,
)
lmProfile

# disp, wt, hp, cyl are variables chosen 

#cross validate glm with caret, cross validated, with reduced variables

ctrl <- trainControl(method = "cv", number = 10)
mdl6 <-
  train(
    mpg ~ am +  disp + wt + hp + cyl,
    family = Gamma,
    method = "glm",
    data = mtcars.fac,
    trControl = ctrl,
    metric =  "Rsquared"
  )

coef6 <- summary(mdl6)$coef
exp(coef6)
exp(confint(mdl6$finalModel))

anova(mdl6$finalModel, test = "Chisq")

#model 7 - try without cyl

mdl7 <-
  train(
    mpg ~ am +  disp + wt + hp,
    family = Gamma,
    method = "glm",
    data = mtcars.fac,
    trControl = ctrl,
    metric =  "Rsquared"
  )

#try without transmission 
mdl8 <-
  train(
    mpg ~ +disp + wt + hp,
    family = Gamma,
    method = "glm",
    data = mtcars.fac,
    trControl = ctrl,
    metric =  "Rsquared"
  )

#test to see if cyl or transmission should be included
anova(mdl8$finalModel, mdl7$finalModel, mdl6$finalModel, test = "Chisq")
anova(mdl7$finalModel, test = "Chisq")
coef7 <- summary(mdl7)$coef
coef8 <- summary(mdl8)$coef

exp(coef7)
exp(confint(mdl7$finalModel))

exp(coef8)
exp(confint(mdl8$finalModel))
#looks like a model with wt & hp is the best, but regardless transmission isn't significant
#confidence intervals for predictions

exp(confint(mdl6$finalModel))
#residual plots - model 6
residuals6 <- resid(mdl6)
predictedValues6 <- predict(mdl6)
resid <- ggplot(data = mtcars.fac, aes(x = mpg, y = residuals6)) +
  geom_point() +
  geom_hline(yintercept = mean(residuals6))
resid

residuals7 <- resid(mdl7)
predictedValues7 <- predict(mdl7)
resid7 <- ggplot(data = mtcars.fac, aes(x = mpg, y = residuals7)) +
  geom_point() +
  geom_hline(yintercept = mean(residuals7))
resid7

#residuals seem dispersed, though there does seem to be an upward slope which is concerning
