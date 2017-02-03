
#In class assignment week 1

setwd("~/Rexamples/Week01")


#19:

#Load data:
ff <- read.csv("chap02q19FireflySpermatophoreMass.csv")

#Basic graph:
hist(ff$spermatophoreMass)

#Better graph:

dev.new(width=6, height=4)
par(lwd = 3)
hist(ff$spermatophoreMass, main="Spermatophore Mass", xlab=list("Spermatophore Mass", cex=1.3), ylab=list("Number of Observations", cex=1.3), lwd=3, col="maroon", cex.axis=1.3)


#26:

#Load data:
tree <- read.csv("chap02q26NeotropicalTreePhotosynthesis.csv")

#Basic graph:
plot(tree$photosyntheticCapacity ~ tree$previousFruits)

#Better graph:
dev.new(width=8, height=6)
par(lwd=2, cex.axis=1.3)
plot(treeplot <- tree$photosyntheticCapacity ~ tree$previousFruits, xlab=list("Fruits in Previous Year", cex=1.3), ylab=list("Photosynthetic Capacity", cex=1.3), pch=24)
points(treeplot, pch=24, col="black", bg="maroon", cex=1.6)
axis(1, lwd=2)
axis(2, lwd=2)

#28:
#Load data:
sneaker <- read.csv("chap02q28SneakerCannibalism.csv")

#Basic graph:
sneakerTable <- table(sneaker$cannibalism, sneaker$typeOfMales)
barplot(sneakerTable, legend.text=T)

#Better graph:
sneaker$typeOfMales <- factor(sneaker$typeOfMales, levels=c("one father", "one sneaker", "multiple sneakers"))
sneaker.table <- table(sneaker)
par(lwd=3, cex=1.3)
barplot(t(sneaker.table), legend.text=TRUE, ylim=c(0,500), lwd=3, xlab=list("Type of Mating"), ylab=list("Number of Observations"))

