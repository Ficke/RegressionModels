require(MASS)
require(dplyr)
?shuttle
shuttle<-shuttle
head(shuttle)
shuttle2 <- mutate(shuttle,auto=1*(use=="auto"))
head(shuttle2)

#question 1
mdl1<-glm(auto~factor(wind)-1,family=binomial,data=shuttle2)
summary(mdl1)$coef
cf<-summary(mdl1)$coef
logodds <- cf[1,1]-cf[2,1]
exp(logodds)

#question 2
mdl2<-glm(auto~factor(wind)+factor(magn)-1,family=binomial,data=shuttle2)
summary(mdl2)
cf2<-summary(mdl2)$coef
logodds2 <- cf2[1,1]-cf2[2,1]
exp(logodds2)

#question 3
mdl3<-glm(I(1-auto)~factor(wind)-1,family=binomial,data=shuttle2)
summary(mdl3)$coef

#question 4
IS<-InsectSprays
mdl4<-glm(count~factor(spray)-1,family = poisson,IS)
cf4<-summary(mdl4)$coef
cf4[1,1]/cf4[2,1]

#question 5


