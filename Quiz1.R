x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

reg <- lm(y~x)
summary(reg)

sigma(reg)


head(mtcars)
cars <- lm(mpg ~ wt, data = mtcars)
newdata <- data.frame(wt = mean(mtcars$wt))

predict(cars,newdata,interval="confidence")



