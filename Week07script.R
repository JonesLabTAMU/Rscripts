#Analysis of covariance, multiple regression

setwd("~/Rexamples/Week07")
parasite_data <- read.csv("parasitedata2012.csv")

#Do females have a different tail height for a given body size compared to males?
#Plot male and female relationships on a single graph

male_data <- subset(parasite_data, Sex=="M")
female_data <- subset(parasite_data, Sex=="F")

dev.new(width=6, height=5)
par(lwd=3)
plot(male_data$THT ~ male_data$SVL, xlab=list("Snout-Vent Length (mm)",cex=1.3), ylab=list("Tail Height (mm)", cex=1.3), pch=15, col="black", cex.axis=1.3, ylim=c(4,14))
box(lwd=3)
axis(1,lwd=3,cex.axis=1.3)
axis(2,lwd=3,cex.axis=1.3)
points(female_data$THT ~ female_data$SVL, pch=17, col="firebrick")

#add regression lines for each sex
male_tht_reg <- lm(male_data$THT ~ male_data$SVL)
female_tht_reg <- lm(female_data$THT ~ female_data$SVL)
abline(male_tht_reg, lwd=3, lty=2)
abline(female_tht_reg, lwd=3, col="maroon")

#They look parallel to me so the interaction in an ancova probably will not be significant
#Run an analysis of covariance
#The covariate should be the first term in the model (svl is the covariate)
tht_ancova <- lm(THT ~ SVL*Sex, data=parasite_data)
anova(tht_ancova)

#The interaction term isn't significant, so rerun the model without it.
#This approach is suitable for ancova, because we were just testing the 
#assumption of no interaction in the model including the interaction term.

tht_ancova <- lm(THT ~ SVL + Sex, data=parasite_data)
anova(tht_ancova)

#Obtain least-squares means for males and females
install.packages("lsmeans", dependencies=TRUE)
library("lsmeans")
tht_lsmeans <- lsmeans(tht_ancova, "Sex")
tht_lsmeans

#Does parasite load have anything to do with tail height?
#One way to address this issue is to look at residual tail height.
#This analysis addresses the question of whether or not 
male_data$resid_THT <- residuals(male_tht_reg)
female_data$resid_THT <- residuals(female_tht_reg)

dev.new(width=6, height=5)
par(lwd=3)
plot(male_data$resid_THT ~ male_data$N_Parasites, xlab=list("Number of Parasites",cex=1.3), ylab=list("Residual Tail Height (mm)", cex=1.3), pch=19, col="black", cex.axis=1.3)
box(lwd=3)
axis(1,lwd=3,cex.axis=1.3)
axis(2,lwd=3,cex.axis=1.3)

resid_m_reg <- lm(resid_THT ~ N_Parasites, data=male_data)
summary(resid_m_reg)
abline(resid_m_reg, lty=2)

dev.new(width=6, height=5)
par(lwd=3)
plot(female_data$resid_THT ~ female_data$N_Parasites, xlab=list("Number of Parasites",cex=1.3), ylab=list("Residual Tail Height (mm)", cex=1.3), pch=19, col="maroon", cex.axis=1.3)
box(lwd=3)
axis(1,lwd=3,cex.axis=1.3)
axis(2,lwd=3,cex.axis=1.3)

resid_f_reg <- lm(resid_THT ~ N_Parasites, data=female_data)
summary(resid_f_reg)
abline(resid_f_reg, lty=2)

#There is a slight, non-significant negative trend for males with more parasites to have smaller tail height values.
#The female data show essentially no relationship.
#This exercise, while instructive, is hardly a valid statistical test. A better approach is to use a full model with Sex, Parasites and SVL as factors.
#Note that we don't care about the interaction between SVL and Sex, because we already established there isn't one and we are just using SVL is a covariate.

#Test for effects of parasites on tail height.
#First, do the sexes differ with respect to N_Parasites?
wilcox.test(N_Parasites ~ Sex, data=parasite_data)

#Apparently not, as the p-value is 0.807 (data are severely non-normal)

full_tht_model <- lm(THT ~ SVL + Sex + N_Parasites + Sex*N_Parasites, data=parasite_data)
anova(full_tht_model)

reduced_tht_model_1 <- lm(THT ~ SVL + Sex + N_Parasites, data=parasite_data)
reduced_tht_model_2 <- lm(THT ~ SVL + Sex, data=parasite_data)
reduced_tht_model_3 <- lm(THT ~ SVL + N_Parasites, data=parasite_data)

anova(reduced_tht_model_1, full_tht_model)
anova(reduced_tht_model_2, reduced_tht_model_1)
anova(reduced_tht_model_3, reduced_tht_model_1)

#Use Type III sums of squares
library(car)
Anova(full_tht_model, type="III")


#Two-factor ANOVA with Type III SS
setwd("~/Rexamples/Week07")
LLsubset <- read.csv("LLplantdatasubset.csv")

library(car)

LLsubset$ln_FN <- log(LLsubset$Fruit_number+1)
LLfullmodel <- lm(ln_FN ~ Microbe_History*Contemp_Water, data=LLsubset)
anova(LLfullmodel)
LLfullmodel <- lm(ln_FN ~ Contemp_Water*Microbe_History, data=LLsubset)
anova(LLfullmodel)

#Compare Models with and Without Interaction Term
LLreducedmodel <- lm(ln_FN ~ Contemp_Water + Microbe_History, data=LLsubset)
anova(LLreducedmodel,LLfullmodel)
#Gives the same p-value as the interaction term p-value, because the interaction term was added last to the model.

#Type III Sum of Squares
Anova(LLfullmodel, type="III")

#Reverse the terms and do it again
LLfullmodel <- lm(ln_FN ~ Microbe_History*Contemp_Water, data=LLsubset)
Anova(LLfullmodel, type="III")

#Same result, because the order doesn't matter for Type III SS
