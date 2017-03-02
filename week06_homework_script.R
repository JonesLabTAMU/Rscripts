#Homework Week06

##### 17_19 #####

setwd("Rexamples/Week06")
plants <- read.csv("chap17q19GrasslandNutrientsPlantSpecies.csv")

#scatterplot: number of species should be the response variable
plot(plants$species ~ plants$nutrients, pch=19, xlab=list("Number of Nutrients Added", cex=1.5), ylab=list("Number of Species",cex=1.5))
box(lwd=3)
axis(1,lwd=3)
axis(2,lwd=3)

#rate of change and standard error
plant_reg <- lm(species ~ nutrients,data=plants)
summary(plant_reg)

#Rate of change is -3.339 species per added nutrient. Standard error is 1.098.

#Add line:
abline(plant_reg, lwd=3, lty=3, col="red")

#Multiple R-squared is 0.536, so 53.6% of the variation in number of plant species is explained by nutrient addition

#The null hypothesis is that the slope is zero. The p-value for our slope is 0.0161, so we can reject the null.


#### 17_20 ####

bmr <- read.csv("chap17q20PrimateMassMetabolicRate.csv")

#Take the natural log of both sides of the equation:
# ln(R) = ln(alpha) + Beta*ln(M)
# In the world of logs, the relationship is now linear, with a slope of Beta and an intercept of ln(alpha).
# To relate this relationship to the data, we need to take the natural log of the explanatory and response variables

bmr$lnmass <- log(bmr$mass)
bmr$lnbmr <- log(bmr$bmr)

# Estimate Beta and call it b

bmr_reg <- lm(lnbmr ~ lnmass, data=bmr)
summary(bmr_reg)

# b is the slope of the regression line, which comes out to 0.74160
# scatterplot:

plot(bmr$lnbmr ~ bmr$lnmass, pch=19, xlab=list("Natural Log of Body Mass", cex=1.5), ylab=list("Natural Log of BMR",cex=1.5))
box(lwd=3)
axis(1,lwd=3)
axis(2,lwd=3)

abline(bmr_reg, lwd=3, lty=3, col="red")

#Estimate the confidence interval for b:

confint(bmr_reg)

# The 95% confidence interval is 0.65-0.83.

#### 17_21 ####

# The point estimate is b=0.74 and the 95% confidence interval includes 0.75, so the slope does not differ from 3/4 at alpha=0.05.

#### 17_24 ####

penguin <- read.csv("chap17q24PenguinTreadmill.csv")
levels(penguin$group) # note that each level has a space after the letters!
penguin$group <- factor(penguin$group,levels=c("BM ","BF ","MF ")) #Do this just to set the order of the groups

means <- tapply(penguin$slope,penguin$group,mean)
stdevs <- tapply(penguin$slope,penguin$group,sd)
n <- tapply(penguin$slope,penguin$group,length)
penguin_table <- data.frame(n,means,stdevs)
colnames(penguin_table) <- c("n", "Mean", "Std.Dev.")
rownames(penguin_table) <- c("Breeding Males", "Breeding Females", "Molting Females")
penguin_table

# Test for a difference in mean slope among groups
# Quick histogram to look for any egregious departures from normality:

library(lattice)
histogram(~slope | group, data=penguin, layout=c(1,3))

# There are very few data points, but things look pretty normal
# An anova is sufficient to compare these slopes (slope estimates are expected to have a normal distribution)

p_aov <- aov(slope ~ group, data=penguin)
anova(p_aov)

# The anova yields a p-value of 0.47, so we cannot reject the null hypothesis that the mean slope is the same across groups.

#### 17_31 ####

lastsupper <- read.csv("chap17q31LastSupperPortionSize.csv")
plot(lastsupper$portionSize ~ lastsupper$year, pch=19, xlab=list("Year", cex=1.5), ylab=list("Portion Size (Heads)",cex=1.5))
box(lwd=3)
axis(1,lwd=3)
axis(2,lwd=3)

supper_reg <- lm(portionSize ~ year, data=lastsupper)
summary(supper_reg)

abline(supper_reg, lty=3, lwd=3, col="maroon")

#The trend is for portion size to increase by 0.003 heads per year.

confint(supper_reg)

#The 95 % CI is (0.0013,0.0053)

#The regression yielded a p-value of 0.0017, so the increase is significantly different from zero.
#Also note that the 95% CI does not include zero.

plot(residuals(supper_reg) ~ year, data=lastsupper, pch=19)

#It looks like the variance is increasing over time. Perhaps a transformation would help. A log transformation does seem to help
#(except for two outliers):

lastsupper$lnportion <- log(lastsupper$portionSize)
plot(lastsupper$lnportion ~ lastsupper$year, pch=19, xlab=list("Year", cex=1.5), ylab=list("Natural Log Portion Size",cex=1.5))
box(lwd=3)
axis(1,lwd=3)
axis(2,lwd=3)
ln_reg <- lm(lnportion ~ year, data=lastsupper)
summary(ln_reg)
abline(ln_reg, lty=3, lwd=3, col="maroon")
plot(residuals(ln_reg) ~ year, data=lastsupper, pch=19)


#### 17_32 ####

snakes <- read.csv("chap17q32CoralSnakeMimics.csv")

plot(proportionAttacksOnMimics ~ distanceFromBoundary, data=snakes, pch=19, xlab=list("Distance from Boundary", cex=1.5), ylab=list("Proportion Attacks on Mimics",cex=1.5))
box(lwd=3)
axis(1,lwd=3)
axis(2,lwd=3)

snake_reg <- lm(proportionAttacksOnMimics ~ distanceFromBoundary, data=snakes)
summary(snake_reg)

abline(snake_reg, lwd=3, lty=2, col="blue")

#The trend is for the proportion of attacks to increase as the distance from the boundary increases.

#Test the hypothesis:
#The p-value for the slope is 0.000776, so we can reject the hypothesis that there is no relationship
#between proportion of attacks and distance from the boundary
