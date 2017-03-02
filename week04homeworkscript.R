#Homework 4 -- Jones

### PROBLEM 11.24 #####

#24a Graph showing distribution

setwd("Rexamples/Week04")
walkingcircles <- read.csv("chap11q24WalkingInCircles.csv")

par(lwd=3)
library(lattice)
histogram(walkingcircles$angle, type="count", breaks=10, xlab = list(label="Median Angle",cex=1.7), ylab = list(label="Number of Observations",cex=1.7), col="firebrick", lwd=3, scales=list(lwd = 3, cex=1.7), xlim=c(-7,7), par.settings = list(axis.line=list(lwd=3)))

#The histogram shows an outlier, so one might want to do a quantile plot
#And also possibly test for a departure from normality

qqnorm(walkingcircles$angle, datax=TRUE)
shapiro.test(walkingcircles$angle)

#The data look pretty non-normal, but this chapter is about the t-test
#So I'll do a t-test anyway


#24b Test whether the mean angle differs from zero

t.test(walkingcircles$angle, mu=0)

#t = -0.31206, df = 14, p-value = 0.7596
#A non-significant t-test almost certainly means there is no difference
#because violating the assumptions of the t-test usually will cause a false
#positive rather than the reverse.

#A non-parametric test might be a sign test for these data. The null
#expectation is zero, so we count the number above and below zero.
#Eight are below zero and 7 are above. Then we use a binomial test:

binom.test(8, 15, p=0.5)

#This test gives a p-value of 0.53, so we still can't reject the null.



#### PROBLEM 12_20 ####

#20a What is the mean difference? What is the 95%CI?

trib <- read.csv("chap12q20ElectricFish.csv")
trib$diff = trib$speciesDownstream - trib$speciesUpstream

#Use t-test to get the mean and std. dev.
#It also does the hypothesis test
t.test(trib$diff)

#Or do exactly the same thing with a paired t-test:
t.test(trib$speciesDownstream,trib$speciesUpstream, paired=TRUE)

#t = 1.9096, df = 11, p-value = 0.0826
#mean difference: 1.833333; 95% CI: -0.2797467 to 3.9464134 


###### PROBLEM 12_27 #############

#27a. Construct a graph showing the change in mosquito activation
#Uses the Ch12, q. 16 data

mosq <- read.csv("chap12q16BeerAndMosquitoes.csv")

#Just want the beer data

beer_mosq <- subset(mosq, drink=="beer")

# I think two histograms would be a good way to show the data
# Here I'll just use the regular "hist" function built in to R

dev.new(width=8, height=4) #Create a new graphical device of a specified width and height
par(lwd=3)
break_vect <- seq(from = 0, to = 1.0, by = 0.1)
hist(beer_mosq$beforeDrink, lwd=3, breaks=break_vect, xlab=list("Mosquito Activation",cex=1.2), col="maroon", main="Before Drink", ylab=list("Number of Observations",cex=1.2), right=FALSE)
dev.new(width=8, height=4) # Creates a new graphical device
par(lwd=3) #set line width to 3
hist(beer_mosq$afterDrink, lwd=3, breaks=break_vect, xlab=list("Mosquito Activation",cex=1.2), col="maroon", main="After Drink", ylab=list("Number of Observations",cex=1.2), right=FALSE)

# Histograms look different, with an increase in the mean after, but neither distribution looks normal
# However, I want to do a paired t-test, so all that matters is that the differences are approximately normal

dev.new(width=8, height=4) # Creates a new graphical device
par(lwd=3) #set line width to 3
break_vect <- seq(from = -0.6, to = 0.6, by = 0.1)
hist(beer_mosq$change, lwd=3, breaks=break_vect, xlab=list("Mosquito Activation",cex=1.2), col="maroon", main="After-Before", ylab=list("Number of Observations",cex=1.2), right=FALSE)
dev.new()
qqnorm(beer_mosq$change, datax=TRUE) # looks pretty normal

# test to see if there's a difference: one-sample t-test on the change

t.test(beer_mosq$change)

# p-value is 7.6e-05, so we can reject the null hypothesis
# The mean change is 0.15 with a 95%CI of 0.087-0.221

#### PROBLEM 12_31 ####

rats <- read.csv("chap12q31RatReciprocity.csv")
stripchart(list(rats$AfterNoHelp, rats$AfterHelp), method="jitter", pch=19, vertical=TRUE, group.names=c("No Help", "Help"), lwd=3, xlab="Treatment", ylab="Pulls per Minute")
box(lwd=3)
rats$diff = rats$AfterHelp-rats$AfterNoHelp
t.test(rats$AfterHelp)
t.test(rats$AfterNoHelp)
t.test(rats$diff)

#Means: AfterHelp = 0.821, AfterNoHelp = 0.602, Mean Diff = 0.219
#Is the difference significantly different from zero:
#Yes. p-value=0.0264, 95%CI for diff: 0.0287-0.409
#Important to randomize order, because not randomizing would 
#confound order and treatment.

#### Problem 13_18 ####

finches <- read.csv("chap13q18ZebraFinches.csv")
wilcox.test(PHA ~ treatment, data=finches)
wilcox.test(SRBC ~ treatment, data=finches)

#Both variables are significantly affected by treatment
#The p-value for PHA is 0.001, and for SRBC it's 0.04
#The assumption is that the data are symmetrical with roughly
#equal variances in the two groups.
#With ties, this test uses a normal approximation to determine
#p-values for the test statistic.

#### Problem 13_30 ####

dengue <- read.csv("chap13q30WolbachiaAndDengue.csv")
stripchart(dengue$viralTiter ~ dengue$strain, method="jitter", pch=19, vertical=TRUE, group.names=c("WB1", "Wild"), lwd=3, xlab="Wolbachia Status", ylab="Viral Titer")
box(lwd=3)

#Signs of non-normality: (1) several observations at zero in WB1,
# (2) A gap in the distribution for Wild.

sorted_dengue <- dengue[order(dengue$viralTiter),]
sorted_dengue

#sorted_dengue is the sorted list

wilcox.test(dengue$viralTiter ~ dengue$strain)

# W=9.5, p = 0.0203, reject the null of no difference

# Log(Y+1) instead of Log(Y) because the data contain zeros.
# The log transformation doesn't matter because it doesn't change the rank order.






