#Example of fitting a spline with a GAM

setwd("~/Rexamples/Week09")
dros_data <- read.csv("dros_growth_rate.csv")

library(mgcv)
d_wt <- dros_data$weight
d_age <- dros_data$age

#Run the model:
#s() means to use a smoother
#fx=FALSE, k=-1 tells it to use cross-validation to determine the amount of smoothing
#bs="cr" tells it to use a cubic regression spline
M3 <- gam(d_wt ~ s(d_age, fx=FALSE, k=-1, bs="cr"))

#plot the estimated curve
plot(M3, se=TRUE)

#plot the data and fitted curve
M3pred <- predict(M3, se=TRUE, type="response")
I1 <- order(d_age)
par(cex=1.5)
plot(d_wt ~ d_age, type="p", xlab=list("Age",cex=1),ylab=list("Weight", cex=1), col="gray")
box(lwd=3)
axis(1,lwd=3)
axis(2,lwd=3)
lines(d_age[I1], M3pred$fit[I1], lty=1, lwd=4)
lines(d_age[I1], M3pred$fit[I1]+2*M3pred$se[I1], lty=2, lwd=2)
lines(d_age[I1], M3pred$fit[I1]-2*M3pred$se[I1], lty=2, lwd=2)

#Run a model with sex as a factor
d_sex <- dros_data$sex
M1 <- gam(d_wt ~ s(d_age,fx=FALSE,k=-1,bs="cr")+factor(d_sex))
summary(M1)
anova(M1)
gam.check(M1)
vis.gam(M1, theta=120, color="heat")

#gam.check indicates some departures from normality and some bad heterogeneity
#plot male and female data on one graph to see how they compare to the vis.gam graph
males <- subset(dros_data, sex=="m")
females <- subset(dros_data, sex=="f")

par(cex=1.5)
plot(females$weight ~ females$age, xlab="Age", ylab="Weight", col="turquoise", pch=19)
points(males$weight ~ males$age, col="firebrick", pch=18)

#It looks like there are really two distinct periods
#During the initial growth period, up to day 54, the sexes are the both the same size
#After day 54, they diverge
#The lines are thus not parallel, so the model likely should include an interaction term

#Construct a GAM with an interaction term
M2 <- gam(d_wt ~ s(d_age,fx=FALSE,k=-1,bs="cr") + s(d_age, by=as.numeric(d_sex=="m"), fx=FALSE, k=-1, bs="cr") + factor(d_sex))
summary(M2)
anova(M2)

#This model shows a significant interaction term and a significant main effect of sex, so 
#the lines are not parallel, but females are larger on average
#Given the way the patter diverges after day 54, you could probably justify analyzing weight up to
#day 54 separately from weight after day 54.

gam.check(M2)
vis.gam(M2, theta=120, color="heat")

#gam.check still shows some problems with this model, so maybe we need to do something else, too.

#Which model fits better: AIC
AIC(M1)
AIC(M2)

# M2 = -205.28, M1=-148.13; This difference is very large, and M2 has the lower AIC
# Negative AICs are fine and the lower (i.e., more negative) one is still preferred

#Which models is a better fit: F test
anova(M1, M2, test="F")

# The models are significantly different, indicating that the interaction is highly significant
# On this basis, M2 is also preferred.



