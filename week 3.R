require(datasets)
require(GGally)
require(ggplot2)

data(swiss)

g <- ggpairs(swiss, lower = list(continuous = wrap("smooth", params = c(method = "loess"))))
g

summary(lm(Fertility ~ ., data = swiss))$coefficients
