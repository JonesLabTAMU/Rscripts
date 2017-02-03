#Week02 Homework Script

setwd("Rexamples/Week02")
males <- read.csv("MaleNewtSampleData.csv")
females <- read.csv("FemaleNewtSampleData.csv")

#Create some tables with summary statistics
#Snout-vent length table
males_svl_table <- c(mean(males$svl),sd(males$svl),sd(males$svl)/sqrt(length(males$svl)),IQR(males$svl))
females_svl_table <- c(mean(females$svl),sd(females$svl),sd(females$svl)/sqrt(length(females$svl)),IQR(females$svl))
table_row_labels <- c("Mean","Std. Dev.","Std. Err.","IQR")
svl_table <- data.frame(table_row_labels,males_svl_table,females_svl_table)
colnames(svl_table) <- c("Quantity","Males","Females")
svl_table

#tl table
males_tl_table <- c(mean(males$tl),sd(males$tl),sd(males$tl)/sqrt(length(males$tl)),IQR(males$tl))
females_tl_table <- c(mean(females$tl),sd(females$tl),sd(females$tl)/sqrt(length(females$tl)),IQR(females$tl))
table_row_labels <- c("Mean","Std. Dev.","Std. Err.","IQR")
tl_table <- data.frame(table_row_labels,males_tl_table,females_tl_table)
colnames(tl_table) <- c("Quantity","Males","Females")
tl_table

#tht table
males_tht_table <- c(mean(males$tht),sd(males$tht),sd(males$tht)/sqrt(length(males$tht)),IQR(males$tht))
females_tht_table <- c(mean(females$tht),sd(females$tht),sd(females$tht)/sqrt(length(females$tht)),IQR(females$tht))
table_row_labels <- c("Mean","Std. Dev.","Std. Err.","IQR")
tht_table <- data.frame(table_row_labels,males_tht_table,females_tht_table)
colnames(tht_table) <- c("Quantity","Males","Females")
tht_table

#mass table
males_mass_table <- c(mean(males$mass),sd(males$mass),sd(males$mass)/sqrt(length(males$mass)),IQR(males$mass))
females_mass_table <- c(mean(females$mass),sd(females$mass),sd(females$mass)/sqrt(length(females$mass)),IQR(females$mass))
table_row_labels <- c("Mean","Std. Dev.","Std. Err.","IQR")
mass_table <- data.frame(table_row_labels,males_mass_table,females_mass_table)
colnames(mass_table) <- c("Quantity","Males","Females")
mass_table

#numberofmates (mating success=ms) table
males_ms_table <- c(mean(males$numberofmates),sd(males$numberofmates),sd(males$numberofmates)/sqrt(length(males$numberofmates)),IQR(males$numberofmates))
females_ms_table <- c(mean(females$numberofmates),sd(females$numberofmates),sd(females$numberofmates)/sqrt(length(females$numberofmates)),IQR(females$numberofmates))
table_row_labels <- c("Mean","Std. Dev.","Std. Err.","IQR")
ms_table <- data.frame(table_row_labels,males_ms_table,females_ms_table)
colnames(ms_table) <- c("Quantity","Males","Females")
ms_table

#numberofoffspring/totalnumbereggs, also known as reproductive success (rs)
males_rs_table <- c(mean(males$numberofoffspring),sd(males$numberofoffspring),sd(males$numberofoffspring)/sqrt(length(males$numberofoffspring)),IQR(males$numberofoffspring))
females_rs_table <- c(mean(females$totalnumbereggs),sd(females$totalnumbereggs),sd(females$totalnumbereggs)/sqrt(length(females$totalnumbereggs)),IQR(females$totalnumbereggs))
table_row_labels <- c("Mean","Std. Dev.","Std. Err.","IQR")
rs_table <- data.frame(table_row_labels,males_rs_table,females_rs_table)
colnames(rs_table) <- c("Quantity","Males","Females")
rs_table

#If you want to export your tables:
write.csv(mass_table,file="newt_mass_table.csv")

#Histograms for Mating Success
dev.new(width=6,height=4)
par(lwd=3)
hist(males$numberofmates, main="Male Mating Success",xlab=list("Number of Mates", cex=1.3),ylab=list("Number of Observations",cex=1.3),lwd=3, breaks=c(-0.5,0.5,1.5,2.5,3.5), col="maroon", cex.axis=1.3)
dev.new(width=6,height=4)
par(lwd=3)
hist(females$numberofmates, main="Female Mating Success",xlab=list("Number of Mates", cex=1.3),ylab=list("Number of Observations",cex=1.3),lwd=3, breaks=c(-0.5,0.5,1.5,2.5,3.5), col="maroon", cex.axis=1.3)

#Histograms for Reproductive Success
dev.new(width=6,height=4)
par(lwd=3)
hist(males$numberofoffspring, main="Male Reproductive Success",xlab=list("Number of Offspring", cex=1.3),ylab=list("Number of Observations",cex=1.3),lwd=3, col="maroon", ylim=c(0,20), right=FALSE, cex.axis=1.3)
dev.new(width=6,height=4)
par(lwd=3)
hist(females$totalnumbereggs, main="Female Reproductive Success",xlab=list("Number of Eggs", cex=1.3),ylab=list("Number of Observations",cex=1.3),lwd=3, col="maroon", xlim=c(0,500),ylim=c(0,20), right=FALSE, cex.axis=1.3)

#Calculate the mean and standard deviation of male and female svl, tl, and tht for each category of mating success and make a table.
m_mean_svl <- tapply(males$svl,males$numberofmates,mean)
m_sd_svl <- tapply(males$svl,males$numberofmates,sd)
m_mean_tl <- tapply(males$tl,males$numberofmates,mean)
m_sd_tl <- tapply(males$tl,males$numberofmates,sd)
m_mean_tht <- tapply(males$tht,males$numberofmates,mean)
m_sd_tht <- tapply(males$tht,males$numberofmates,sd)
ms_vect <- c(0:3)
male_table_by_ms <- data.frame(ms_vect,m_mean_svl,m_sd_svl,m_mean_tl,m_sd_tl,m_mean_tht,m_sd_tht)
colnames(male_table_by_ms) <- c("Mating Success","Mean SVL","St.Dev. SVL", "Mean TL", "St.Dev. TL", "Mean THT", "St.Dev. THT")

f_mean_svl <- tapply(females$svl,females$numberofmates,mean)
f_sd_svl <- tapply(females$svl,females$numberofmates,sd)
f_mean_tl <- tapply(females$tl,females$numberofmates,mean)
f_sd_tl <- tapply(females$tl,females$numberofmates,sd)
f_mean_tht <- tapply(females$tht,females$numberofmates,mean)
f_sd_tht <- tapply(females$tht,females$numberofmates,sd)
ms_vect <- c(0:3)
female_table_by_ms <- data.frame(ms_vect,f_mean_svl,f_sd_svl,f_mean_tl,f_sd_tl,f_mean_tht,f_sd_tht)
colnames(female_table_by_ms) <- c("Mating Success","Mean SVL","St.Dev. SVL", "Mean TL", "St.Dev. TL", "Mean THT", "St.Dev. THT")

###### Plot reproductive success on mating success for males and females ######

dev.new(width=6,height=5)
plot(males$numberofoffspring ~ males$numberofmates, xlab=list("Mating Success",cex=1.3), ylab=list("Reproductive Success", cex=1.3), pch=19, cex.axis=1.3)
box(lwd=3)
axis(1,lwd=3, cex.axis=1.3)
axis(2,lwd=3, cex.axis=1.3)

dev.new(width=6,height=5)
plot(females$totalnumbereggs ~ females$numberofmates, xlab=list("Mating Success", cex=1.3), ylab=list("Reproductive Success", cex=1.3), pch=19, ylim=c(0,450), cex.axis=1.3)
box(lwd=3)
axis(1,lwd=3, cex.axis=1.3)
axis(2,lwd=3, cex.axis=1.3)

###### Function to calculate opportunity for sexual selection ######
oss <- function(x) {
	y <- mean(x)
	z <- var(x)
	i <- z/(y*y)
	return(i)
}

oss(males$numberofmates)
oss(females$numberofmates)

#Answers:
#(1) Newts are sexually dimorphic. Males are larger than females. Tail length is especially different -- male tails are 70 percent taller than those of females.
#(2) The mating success and reproductive success histograms are different -- males have a lot more zeros in particular.
#(3) There aren't any obvious differences among groups based on mating success.
#(4) The plot of reproductive success on mating success does differ between the sexes -- males show an increase in reproductive success with more mates and females don't.
#(5) The male opportunity for sexual selection is about 60 percent higher than that of females.



