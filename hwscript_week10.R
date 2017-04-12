# Analysis of gambusia data from O'Dea et al.

setwd("~/Rexamples/Week10")
gamb <- read.csv("gambusia_individual_offspring.csv")

#Offspring are nested within mothers
#Consequently, mother should be included as a random factor
#First take a look at the residuals

M1.lm <- lm(Indiv_Offspring_Size ~ Age*Size, data=gamb)
summary(M1.lm)
par(mfrow=c(2,2))
plot(M1.lm)

#One of the treatments has outliers on both ends
hist(residuals(M1.lm))

#Except for the outliers, everything looks normal
gamb$logOS <- log(gamb$Indiv_Offspring_Size)
M2.lm <- lm(logOS ~ Age*Size, data=gamb)
summary(M2.lm)
par(mfrow=c(2,2))
plot(M2.lm)
hist(residuals(M2.lm))

#The log transformation didn't help really, so stick with the untransformed data

#Does the female ID affect the values of offspring size? Do a boxplot to see.
E <- rstandard(M1.lm)
boxplot(E ~ Female_ID, data=gamb, axes=FALSE)
abline(0, 0); axis(2)
text(1:141, -2.5, levels(gamb$Female_ID), cex=0.5, srt=65)

#This plot also identifies some of the females whose broods contain outliers

#Run a generalized linear regression with the same variables as the lm
library(nlme)
M.gls <- gls(Indiv_Offspring_Size ~ Age*Size, data=gamb)
summary(M.gls)

#Fit a linear mixed model
M1.lme <- lme(Indiv_Offspring_Size ~ Age*Size, random = ~1 | Female_ID, method="REML", data=gamb)
anova(M.gls, M1.lme)

#The fit with the random term is WAAAAAY better
#Validate the model with this random structure
E2 <- resid(M1.lme, type="normalized")
F2 <- fitted(M1.lme)
op <- par(mfrow = c(2,2), mar=c(4,4,3,2))
MyYlab = "Residuals"
plot(x=F2, y=E2, xlab="Fitted values", ylab=MyYlab)
boxplot(E2 ~ Age, data=gamb, main="Age", ylab=MyYlab)
boxplot(E2 ~ Size, data=gamb, main="Size", ylab=MyYlab)
par(op)

#The model validation looks really good actually (but there are still a couple of outliers)
#Find the optimal fixed structure
summary(M1.lme)

#The first candidate to remove is the Age:Size interaction

M1.Full <- lme(Indiv_Offspring_Size ~ Age*Size, random = ~1 | Female_ID, method="ML", data=gamb)
M1.A <- lme(Indiv_Offspring_Size ~ Age+Size, random = ~1 | Female_ID, method="ML", data=gamb)
anova(M1.Full, M1.A)

#In M1.A, Age is also not significant, so drop that, too
M1.A1 <- lme(Indiv_Offspring_Size ~ Size, random = ~1 | Female_ID, method="ML", data=gamb)
anova(M1.A, M1.A1)

#The final model has just Size as a fixed effect and Female ID as a random effect
M1.Full_REML <- lme(Indiv_Offspring_Size ~ Size, random = ~1 | Female_ID, method="REML", data=gamb)
summary(M1.Full_REML)

#And... It's not significant.
#Model Validation
E2 <- resid(M1.Full_REML, type="normalized")
F2 <- fitted(M1.Full_REML)
op <- par(mfrow = c(2,2), mar=c(4,4,3,2))
MyYlab = "Residuals"
plot(x=F2, y=E2, xlab="Fitted values", ylab=MyYlab)
boxplot(E2 ~ Size, data=gamb, main="Size", ylab=MyYlab)
par(op)

#Produce a boxplot showing different means for the treatment categories
boxplot(Indiv_Offspring_Size ~ Age+Size, data=gamb, xlab="Treatment Combination", ylab="Offspring Size")

#All of this seems consistent with the notion that neither Age nor Size affects offspring size
library(lsmeans)
gamb_lsmeans <- lsmeans(M1.A, ~ Size*Age, data=gamb)
gamb_lsmeans
plot(gamb_lsmeans)

#If there's any trend at all, it's that the larger females (B) have smaller offspring than the smaller females (S).
#However, we cannot be confident in this result, because the p-value is 0.07 from our GLMM.


#Part two -- the effects of female size and age on number of offspring
setwd("~/Rexamples/Week12")
gamb2 <- read.csv("gambusia_female_means.csv")
gamb2 <- gamb2[1:140,] #remove missing row at the end
gamb2$Age <- factor(gamb2$Age)
gamb2$Size <- factor(gamb2$Size)


#What is the relationship between female size and number of offspring?
plot(Number_of_Offspring ~ Female_Size, data=gamb2)
M1.lm <- lm(Number_of_Offspring ~ Female_Size, data=gamb2)
plot(M1.lm)

#Perhaps there's some heterogeneity, with the variance increasing with female size
plot(Number_of_Offspring ~ Age, data=gamb2) # Variance looks larger with Old but maybe it's driven by size

#Linear model with Female Size and Age as the factors
M2.lm <- lm(Number_of_Offspring ~ Female_Size*Age, data=gamb2)
summary(M2.lm)
plot(M2.lm)

#This model might be better fit with a model that includes heterogeneity, so it calls for a GLS.
M.gls <- gls(Number_of_Offspring ~ Female_Size*Age, data=gamb2)

#Fit different variance structures based on Female_Size
vf1 <- varPower(form = ~Female_Size)
M.gls1 <- gls(Number_of_Offspring ~ Female_Size*Age,weights=vf1,data=gamb2)

vf2 <- varExp(form = ~Female_Size)
M.gls2 <- gls(Number_of_Offspring ~ Female_Size*Age,weights=vf2,data=gamb2)

vf3 <- varConstPower(form = ~Female_Size)
M.gls3 <- gls(Number_of_Offspring ~ Female_Size*Age,weights=vf3,data=gamb2)

anova(M.gls, M.gls1, M.gls2, M.gls3) #looks like the best is gls2, confirm with LRtest
anova(M.gls,M.gls2) #p < .0001

plot(M.gls2) #looks like the heterogeneity problem is solved

#Does it help to also model variance by level of Age
vf4 <- varComb(varIdent(form=~1|Age),varExp(form = ~Female_Size))
M.gls4 <- gls(Number_of_Offspring ~ Female_Size*Age,weights=vf4,data=gamb2)

anova(M.gls2,M.gls4) #p = 0.3528 so it doesn't help

#So M.gls2 is my model. Now look at the results

summary(M.gls2)

#The interaction is non-significant, so remove it
vf2 <- varExp(form = ~Female_Size)
M.gls5 <- gls(Number_of_Offspring ~ Female_Size + Age,weights=vf2, method="ML", data=gamb2)
M.gls6 <- gls(Number_of_Offspring ~ Female_Size * Age,weights=vf2, method="ML", data=gamb2)
M.gls7 <- gls(Number_of_Offspring ~ Female_Size,weights=vf2, method="ML", data=gamb2)
anova(M.gls6,M.gls5,M.gls7)

#Refit the final model with REML
M.gls8 <- gls(Number_of_Offspring ~ Female_Size,weights=vf2, method="REML", data=gamb2)
summary(M.gls8)

#At the end of the day, only female size predicts offspring number
plot(M.gls8)

boxplot(Number_of_Offspring ~ Size * Age, data=gamb2)

#However, their experimental design included Size as a categorical variable
#What happens if we do that?

#It looks like big females have a greater variance in offspring number.
#We can construct models with and without taking that into account

M1.cat <- gls(Number_of_Offspring ~ Size * Age, data=gamb2)
plot(M1.cat)
summary(M1.cat)

vf10 <- varIdent(form = ~1 | Size)
M2.cat <- gls(Number_of_Offspring ~ Size*Age, weights=vf10, data=gamb2)
summary(M2.cat)
anova(M1.cat, M2.cat)
plot(M2.cat)

#Modeling the residuals does fix the heterogeneity. Now the interaction is
#no longer significant, but age and size are both significant.

M3.cat <- gls(Number_of_Offspring ~ Size + Age, weights=vf10, method="ML", data=gamb2)
M4.cat <- gls(Number_of_Offspring ~ Size*Age, weights=vf10, method="ML", data=gamb2)
M5.cat <- gls(Number_of_Offspring ~ Size, weights=vf10, method="ML", data=gamb2)
anova(M4.cat,M3.cat,M5.cat)
summary(M5.cat)

#The best model includes only Size -- bigger females have more offspring
M6.cat <- gls(Number_of_Offspring ~ Size, weights=vf10, method="REML", data=gamb2)
summary(M6.cat)

#Size definitely has an effect -- there may be an interaction between size and age.
#The effect of age is not very convincing
#Look at the boxplots to see:

boxplot(Number_of_Offspring ~ Size + Age, data=gamb2)
boxplot(Number_of_Offspring ~ Age, data=gamb2)
boxplot(Number_of_Offspring ~ Size, data=gamb2)

