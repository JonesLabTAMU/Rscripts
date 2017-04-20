#Week 11 Exercise
#Part I: Janicke_et_al_2015 -- mating success in hermaphrodites
#The Treatment is a feeding treatment (high and low food)
#Body weight is another factor
#And finally mating success is the other factor of interest

#Fit a Poisson and a Negative Binomial

setwd("~/Rexamples/Week11")
janicke <- read.csv("Janicke_et_al_2015_Am_Nat_DATA.csv")

#Examine the histograms of mating success for males and females
dev.new(width=6,height=5)
par(lwd=3, cex=1.2)
hist(janicke$mMS, breaks=c(-0.5,0.5,1.5,2.5,3.5,4.5), main = "", ylab="Number", xlab="Male MS", col="maroon")
axis(1,lwd=3)
axis(2,lwd=3)

dev.new(width=6,height=5)
par(lwd=3, cex=1.2)
hist(janicke$fMS, breaks=c(-0.5,0.5,1.5,2.5,3.5,4.5), main = "", ylab="Number", xlab="Male MS", col="turquoise")
axis(1,lwd=3)
axis(2,lwd=3)

#Is body weight associated with male or female mating success
dev.new(width=6,height=5)
par(cex=1.2)
plot(mMS ~ BodyWeight, data=janicke, pch=19, xlab="Body Weight", ylab="Mating Success")
box(lwd=3)
axis(1,lwd=3)
axis(2,lwd=3)

dev.new(width=6,height=5)
par(cex=1.2)
plot(fMS ~ BodyWeight, data=janicke, pch=19, xlab="Body Weight", ylab="Mating Success")
box(lwd=3)
axis(1,lwd=3)
axis(2,lwd=3)

#No obvious patterns

#Start by analyzing mMS
#Run a poisson glm
M1 <- glm(mMS ~ BodyWeight*Treatment, family=poisson, data=janicke)
summary(M1)

#The data appear to be underdispersed D/(n-p) =~ 0.5
#A quasipoisson can deal with this situation

M2 <- glm(mMS ~ BodyWeight*Treatment, family=quasipoisson, data=janicke)
summary(M2)

drop1(M2, test="F")

#The interaction is non-significant
M3 <- glm(mMS ~ BodyWeight + Treatment, family=quasipoisson, data=janicke)
summary(M3)
drop1(M3, test="F")

#Body weight is also not significant
M4 <- glm(mMS ~ Treatment, family=quasipoisson, data=janicke)
summary(M4)
plot(M4)
plot(M3)

#The diagnostics look okay -- from this model I would conclude that neither body weight nor
#food treatment affects mating success

#Test a neg binomial to see what happens
library(MASS)
M6 <- glm.nb(mMS ~ BodyWeight*Treatment, link="log", data=janicke)
summary(M6)

#Warnings and a rediculous value of theta (the dispersion parameter), indicating that this model
#will not fit the data. A negative binomial cannot deal with underdispersion but a quasipoisson can.

#However, the quasipoisson looked okay, and it shows no significant effects of body size or treatment on mating success
#What about fMS?

M7 <- glm(fMS ~ BodyWeight*Treatment, family=poisson, data=janicke)
summary(M7)

#The data appear to be underdispersed
#A quasipoisson can deal with this situation

M8 <- glm(fMS ~ BodyWeight*Treatment, family=quasipoisson, data=janicke)
summary(M8)
plot(M8)

M8.5 <- glm(fMS ~ Treatment, family=quasipoisson, data=janicke)
summary(M8.5)
plot(M8.5)

#Treatment has no significant effect on fMS. The model does seem to fit reasonably well, too.

#Treating Body Weight as a random factor. Use glmer in lme4 -- have to use a regular Poisson because quasipoisson
#is not supported.
library(lme4)
M.glmer1 <- glmer(mMS ~ Treatment + (1 | BodyWeight), family=poisson, data=janicke)
summary(M.glmer1)

M.glm0 <- glm(mMS ~ Treatment, family=poisson, data=janicke)
summary(M.glm0)

#The model without the random factor has the better AIC, so keep the model without the random
#factor and fit a quasipoisson

M.glmer1f <- glmer(fMS ~ Treatment + (1 | BodyWeight), family=poisson, data=janicke)
summary(M.glmer1f)

M.glm0f <- glm(fMS ~ Treatment, family=poisson, data=janicke)
summary(M.glm0f)

#Same interpretation here -- the random factor is not helping. Since the quasipoisson is a better
#fit, we should go back to that model (i.e., the quasipoisson glm)

#At the end of the day, we have no evidence that either body weight or treatment are related to mating success


#What about mRS and fRS?

dev.new(width=6,height=5)
par(lwd=3, cex=1.2)
hist(janicke$mRS, main = "", ylab="Number", xlab="Male MS", col="maroon")
axis(1,lwd=3)
axis(2,lwd=3)

dev.new(width=6,height=5)
par(lwd=3, cex=1.2)
hist(janicke$fRS, main = "", ylab="Number", xlab="Male MS", col="turquoise")
axis(1,lwd=3)
axis(2,lwd=3)

dev.new(width=6,height=5)
par(cex=1.2)
plot(mRS ~ BodyWeight, data=janicke, pch=19, xlab="Body Weight", ylab="Mating Success")
box(lwd=3)
axis(1,lwd=3)
axis(2,lwd=3)

dev.new(width=6,height=5)
par(cex=1.2)
plot(fRS ~ BodyWeight, data=janicke, pch=19, xlab="Body Weight", ylab="Mating Success")
box(lwd=3)
axis(1,lwd=3)
axis(2,lwd=3)

#No really clear patterns, but perhaps these things have a poisson or negative binomial distribution

M9 <- glm(mRS ~ BodyWeight*Treatment, family=poisson, data=janicke)
summary(M9)
plot(M9)

#Fit doesn't seem awful, but the data are way overdispersed
M10 <- glm(mRS ~ BodyWeight*Treatment, family=quasipoisson, data=janicke)
summary(M10)
plot(M10)

#This model seems to fit pretty well, but the dispersion parameter is quite large at 18.7
#A negative binomial may provide a better fit
M11 <- glm.nb(mRS ~ BodyWeight*Treatment, link="log", data=janicke)
summary(M11)

drop1(M11, test="Chi")
M12 <- glm.nb(mRS ~ BodyWeight + Treatment, link="log", data=janicke)
summary(M12)
plot(M12)

M13 <- glm.nb(mRS ~ Treatment, link="log", data=janicke)
summary(M13)

boxplot(mRS ~ Treatment, data=janicke)
#High food individuals have higher male reproductive success than low food individuals, p = 0.009

#Female reproductive success
M14 <- glm(fRS ~ BodyWeight*Treatment, family=poisson, data=janicke)
summary(M14)
plot(M14) 

M15 <- glm(fRS ~ BodyWeight*Treatment, family=quasipoisson, data=janicke)
summary(M15)
plot(M15)

M16 <- glm.nb(fRS ~ BodyWeight*Treatment, link="log", data=janicke)
summary(M16)
plot(M16)
#negative binomial doesn't seem to fit, so stick with the quasipoisson

drop1(M15, test="F")
M17 <- glm(fRS ~ BodyWeight+Treatment, family=quasipoisson, data=janicke)
summary(M17)
plot(M17)
drop1(M17, test="F")


#This model suggests body weight and treatment are both significant
#Check to see whether the model actually seems to fit the data
#Plot fRS on body weight for each treatment
high_data <- subset(janicke, Treatment=="HIGH")
low_data <- subset(janicke, Treatment=="LOW")

M18 <- glm(fRS ~ BodyWeight, family=quasipoisson, data=high_data)
dev.new(width=6,height=5)
par(cex=1.2)
plot(fRS ~ BodyWeight, data=high_data, pch=19, ylab="Female RS", xlab="Body Weight")
box(lwd=3); axis(1,lwd=3); axis(2,lwd=3)
MyData <- data.frame(BodyWeight = seq(from = 40, to = 110, by=5))
G <- predict(M18, newdata=MyData, type="link", se=TRUE)
F <- exp(G$fit)
FSEUP <- exp(G$fit+1.96*G$se.fit)
FSEDOWN <- exp(G$fit-1.96*G$se.fit)
lines(MyData$BodyWeight,F,lty=1,lwd=3)
lines(MyData$BodyWeight,FSEUP,lty=2,lwd=3)
lines(MyData$BodyWeight,FSEDOWN,lty=2,lwd=3)

M19 <- glm(fRS ~ BodyWeight, family=quasipoisson, data=low_data)
dev.new(width=6,height=5)
par(cex=1.2)
plot(fRS ~ BodyWeight, data=low_data, pch=19, ylab="Female RS", xlab="Body Weight")
box(lwd=3); axis(1,lwd=3); axis(2,lwd=3)
MyData <- data.frame(BodyWeight = seq(from = 40, to = 110, by=5))
G <- predict(M19, newdata=MyData, type="link", se=TRUE)
F <- exp(G$fit)
FSEUP <- exp(G$fit+1.96*G$se.fit)
FSEDOWN <- exp(G$fit-1.96*G$se.fit)
lines(MyData$BodyWeight,F,lty=1,lwd=3)
lines(MyData$BodyWeight,FSEUP,lty=2,lwd=3)
lines(MyData$BodyWeight,FSEDOWN,lty=2,lwd=3)

#Two things are happening here. First, it looks like the fit is almost linear
#So the quasipoisson isn't really necessary.
#Second, it looks like the variance is larger in the low food treatment,
#suggesting that we should model the variance as a function of food treatment.

#Now we are back to a gls model from Week 10.

library(nlme)
M.lm <- gls(fRS ~ BodyWeight*Treatment, data=janicke)
vf1 <- varIdent(form = ~1 | Treatment)
M.gls1 <- gls(fRS ~ BodyWeight*Treatment, data=janicke, weights=vf1)
anova(M.lm, M.gls1)

summary(M.lm)
summary(M.gls1)
plot(M.lm)
plot(M.gls1)

M.gls2 <- gls(fRS ~ BodyWeight+Treatment, data=janicke, weights=vf1)
summary(M.gls2)

#To compare the models with and without interaction term, use ML estimation
M.gls1ME <- gls(fRS ~ BodyWeight*Treatment, data=janicke, method = "ML", weights=vf1)
M.gls2ME <- gls(fRS ~ BodyWeight+Treatment, data=janicke, method = "ML", weights=vf1)
anova(M.gls1ME, M.gls2ME)

plot(fRS ~ BodyWeight, data=janicke)

#Include BodyWeight as a random factor
M.lmT <- gls(fRS ~ Treatment, method="REML", weights=vf1, data=janicke)
summary(M.lmT)
M.lme1 <- lme(fRS ~ Treatment, random = ~1 | BodyWeight, method="REML", weights=vf1, data=janicke)
summary(M.lme1)
M.lme2 <- lme(fRS ~ Treatment, random = ~1 + Treatment | BodyWeight, method="REML", weights=vf1, data=janicke)
summary(M.lme2)

#Does modeling the heterogeneity improve the fit
M.lme3 <- lme(fRS ~ Treatment, random = ~1 | BodyWeight, method="REML", data=janicke)
summary(M.lme3)
M.gls4 <- gls(fRS ~ Treatment, method="REML", data=janicke)
summary(M.gls4)

anova(M.gls4, M.lme3) #Adding the random factor to a regular linear model is not better
anova(M.gls4, M.lmT)  #Adding the weights is better


anova(M.lmT, M.lme1, M.lme2)
#Including body weight as a random factor doesn't really improve the fit

plot(M.lmT)
plot(M.lme1)
plot(M.lme2)

#Basically, we end up back a square one, with a singe-factor anova with a bit of heterogeneity due to treatment:
#Final model:

M.lm_final <- gls(fRS ~ Treatment, method="REML", weights=vf1, data=janicke)
summary(M.lm_final)

plot(fRS ~ Treatment, data=janicke)







