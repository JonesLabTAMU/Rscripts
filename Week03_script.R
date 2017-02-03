setwd("~/Rexamples/Week03")
#Ch. 7, Problem 21
#One classical experiment on ESP (extrasensory perception) tests for the 
#ability of an individual to show telepathy--to read the mind of another 
#individual. This test uses five cards with different designs, all known to both participants. 
#In a trial, the "sender" sees a randomly chosen card and concentrates
#on its design. The "receiver" attempts to guess the identity of the card.
#Each of the five cards is equally likely to be chosen, 
#and only one card is the correct answer at any point.
#a. Out of 10 trials, a receiver got four cards correct. 
#What is her success rate? What is her expected rate of success, assuming
#she is only guessing?

success.rate<-4/10
success.rate
exp.rate<-1/5
exp.rate

#b. Is her higher actual success rate reliable evidence that the receiver 
#has telepathic abilities? Carry out the appropriate hypothesis test.

binomial_result<-binom.test(4,n=10,p=exp.rate)
binomial_result

#**Her actual success rate is higher than expected.
#**However, her success rate is not significantly different from expectations.
#**The p-value is 0.12 and the expected value falls within the 95% CI.
#**We cannot reject the null hypothesis that she's guessing.


#c. Assume another (extremely hypothetical) individual tried to guess the ESP 
#cards 1000 times and was correct 350 of those times. This is very 
#significantly different from the chance rate, yet the proportion of her 
#successes is lower than the individual in part (a). Explain this apparent 
#contradiction.

binomial_res1000 <- binom.test(350, n=1000, p=exp.rate)
binomial_res1000

#**This contradiction is due to sampling error and probability.
#**With a very large sample size, even a small departure from the expected
#**proportion can be statistically significant.
#**In this type of experiment, a result like this could be caused by
#**some sort of experimenter bias or *REAL ESP*.

#Ch. 8 Problem 14
#In North America, between 100 million and 1 billion birds die each year by
#crashing into windows on buildings, more than any other human-related cause.
# This figure represents up to 5% of all birds in the area. One possible
#solution is to construct windows angled downward slightly, so that they
#reflect the ground rather than an image of the sky to a flying bird.
#An experiment by Klem et al. (2004) compared the number of birds that died
#as a result of verticle windows, windows angled 20 degrees off vertical, 
#and windows angled 40 degrees off vertical. The angles were randomly assigned
#with equal probability to six windows and changed daily; assume for this 
#exercise that windows and window locations were identical in every respect
#except angle. Over the course of the experiment, 30 birds were killed by
#windows in the vertical orientation, 15 were killed by windows set at 20
#degrees off vertical, and 8 were killed by windows set at 40 degrees off
#vertical.
#a. Clearly state an appropriate null hypothesis and an alternative hypothesis.

#**H0: Window angle does not affect the death rates of birds.
#**H1: Window angle affects the death rates of birds.

#b. What proportion of deaths occurred while the windows were set at a 
#vertical orientation?
birds<-read.csv("chap08q14BirdWindowCrash.csv")
bird.deaths<-table(birds)
bird.deaths

total.deaths<-sum(bird.deaths)
death.props<-bird.deaths/total.deaths
death.props

#**0.5660377 bird deaths are attributed to vertical windows.

#c. What statistical test would you use to test the null hypothesis?

#**Chi-square goodness of fit

#d. Carry out the statistical test from part (c). Is there evidence that
#window angle affects mortality rates of birds?

#Try to figure out what the p=c(rep((... command is doing
bird.chi<-chisq.test(bird.deaths,p = c(rep((total.deaths/3),3))/total.deaths)
bird.chi

#Or another way:
bird.chi<-chisq.test(bird.deaths, p = c(1/3,1/3,1/3))
bird.chi

#**yes window angle affects mortality rates

#Ch. 9 Problem 21
#Aging workers of the Neotropical termite, Neocapritermes taracua, 
#develop blue crystal-containing glands ("backpacks") on their backs.
#When they fight intruding termites and are hampered, these "blue" termites 
#explode, and the glands spew a sticky liquid (Sobotnik et al. 2012). 
#The following data are from an experiment that measured the toxicity of the 
#blue busbstance. A single drop of the liquid extracted from blue termites
#was placed on individuals of a second termite species, Labiotermes labralis,
#and the number that were immobilized (dead or paralyzed) within 60 minutes
#was recorded. The frequency of this outcome was compared with a control
#treatment in which liquid from glands of "white" termites lacking the blue
#crystals was dropped instead. Is the blue liquid toxic compared to liquid 
#from white termites?

termites <- read.csv("chap09q21BlueTermites.csv")
termites$immobilization <- factor(termites$immobilization, levels=c("unharmed","immobilized"))
termites$glandColor <- factor(termites$glandColor, levels = c("white","blue"))
termite.table<-table(termites)
par(lwd=3)
mosaicplot(termite.table, col = c("maroon", "gray"), cex=1.3, main = NULL, xlab=list("gland color", cex=1.3), ylab=list("immobilization status", cex=1.3)) 

par(lwd=3, cex=1.4)
plot(termites$glandColor, termites$immobilization, cex=1.3, xlab="Gland Color", ylab="Immobilization Status", col=c("black","firebrick"))
axis(4, lwd = 2)

# If you want to calculate an odds ratio
# install epitools if you haven't already. 
# You might have to run R as an administrator for this installation to work.
install.packages("epitools", dependencies = TRUE)

library(epitools)
termite.or<-oddsratio(termite.table, method = "wald")$measure[-1,]
termite.chi<-chisq.test(termites$glandColor, termites$immobilization, correct = FALSE)
termite.or
termite.chi

#**YES blue liquid is toxic compared to liquid from white termites. The odds ratio is 42.5
# and its 95% CI does not include 1. The chi-squared test p-value is 2.4e-10, which is highly
# significant.

#Ch. 9 Problem 24
#Male Drosophila become sterile when exposed to moderately high temperatures, 
#because sperm are damaged by heat at much lower temperatures than other cells.
#Rohmer et al. (2004) asked whether flies from warmer climates are adapted
#to higher temperatures. They collected flies from France (where it is 
#relatively cool) and India (relatively warm) to test the effects of 
#temperature on sterility. In one procedure, they raised male flies from both
#locations at a high temperature, 30.5 C, and recorded whether the flies were 
#sterile or fertile. Thirty-two out of 50 flies from France were sterile at 
#this temperature, whereas 20 of 50 flies from India were sterile.

#a. Is this an observational or experimental study?
#**experimental

#b. Draw a graph to illustrate the association between sterility and source 
#location (India vs France). What association is suggested?

fly.dat<-read.csv("chap09q24HeatSterility.csv")

fly.table<-table(fly.dat)

par(lwd=3, cex=1.4)
plot(fly.dat$location, fly.dat$sterility, xlab = "Location", ylab = "Sterility", col=c("maroon","gold"))
axis(4, lwd=2)

# Save the plot as a ".png" file:
png("FlyHeatSterility.png",height=7,width=7,units="in",res=300) 
par(lwd=3, cex=1.4)
plot(fly.dat$location, fly.dat$sterility, xlab = "Location", ylab = "Sterility", col=c("maroon","gold"))
axis(4, lwd=2)
dev.off()
#**Indian flies seem to be better able to remain fertile at high temperatures.
 
#c. Is there evidence that the populations of flies from these two locations
#differ in their probability of sterility at this temperature?
#Do the appropriate hypothesis test.

fly.chi<-chisq.test(fly.dat$location, fly.dat$sterility, correct = TRUE) 
fly.chi
#**p=0.02768
fly.fishers<-fisher.test(fly.dat$location, fly.dat$sterility)
fly.fishers
#**p=0.02718
#yes the flies differ in their probability of sterility -- we can reject the null

#d. Estimate the relative risk of sterility at this temperature in the Indian
#population compared to the population in France (consider the Indian 
#population to be the treatment group for this analysis). Include a 95% 
#confidence interval.

#Calculate the risk ratio manually: We want the risk of sterility in the Indian
#population compared to the French population. So we want to divid the Indian
#value by the French value.

a <- fly.table[4]
c <- fly.table[2]
b <- fly.table[3]
d <- fly.table[1]

P1 <- a/(a+c)
P2 <- b/(b+d)

P1
P2

ln.fly.rr <- log(P1/P2)
fly.rr <- exp(ln.fly.rr)
fly.rr

# Calculate the 95% CI for RR:

se.ln.rr <- sqrt(1/a+1/b-1/(a+c)-1/(b+d))
ln.lower <- ln.fly.rr - 1.96*se.ln.rr
ln.upper <- ln.fly.rr + 1.96*se.ln.rr

lowerCI <- exp(ln.lower)
upperCI <- exp(ln.upper)

fly.rr
lowerCI
upperCI

# The estimate of relative risk is 0.625, and the 95% CI is (0.4198, 0.9306).
# Relative risk is 0.625, so flies from India are less likely to be sterile at the
# high temperature.

# Use the riskratio function:

fly.risk<-riskratio(fly.table, rev = "both", method = "wald")$measure[-1,]
fly.risk
#**Risk of sterility at the high temperature is 0.600, with a confidence
#**interval from 0.389 to 0.925. Note that these values are different from
# the manually calculated ones because this function uses a different method.
# Use ?riskratio to see the help page and make sure your table has the rows
# and columns in the proper order to get the test you want.

