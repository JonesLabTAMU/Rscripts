# Perform a two-factor analysis of variance on fruit number in the 
# Lau and Lennon data set

######### Fruit Number ANOVA (Fig 1A) ##########

setwd("Rexamples/Week05")
plantdata <- read.csv("LLplantdata.csv")
keepers <- c("Microbe_History", "Contemp_Water", "Fruit_number", "Flower_number", "Days_to_flower")
less_plant_data <- plantdata[keepers] # Reduce the dataset to columns we need
less_plant_data <- less_plant_data[1:320,] # Remove blank row

less_plant_data$Microbe_History <- factor(less_plant_data$Microbe_History) # Redo the factors to omit the blank
levels(less_plant_data$Microbe_History) # Check to make sure the factor has two levels
less_plant_data$Contemp_Water <- factor(less_plant_data$Contemp_Water) # refactor Contemp_Water, too
levels(less_plant_data$Contemp_Water)

nona_fruit_data <- less_plant_data[1:3] # Reduce the data again to just include fruit number as a response
nona_fruit_data <- subset(nona_fruit_data, Fruit_number > -1) # Remove rows with "NA"
nona_fruit_data$log_fn <- log(nona_fruit_data$Fruit_number + 1) # add a column with ln fruit number


# Calculate mean and sd
fn_means <- tapply(nona_fruit_data$Fruit_number,INDEX=list(nona_fruit_data$Contemp_Water,nona_fruit_data$Microbe_History),mean)
fn_stdev <- tapply(nona_fruit_data$Fruit_number,INDEX=list(nona_fruit_data$Contemp_Water,nona_fruit_data$Microbe_History),sd)
fn_length <- tapply(nona_fruit_data$Fruit_number,INDEX=list(nona_fruit_data$Contemp_Water,nona_fruit_data$Microbe_History),length)
fn_sem <- fn_stdev/sqrt(fn_length)
fn_means
fn_sem

fn_means_vect <- fn_means[1:4]
means_labels_vect = c("Dry Microbes\nDry Soil","Dry Microbes\nWet Soil", "Wet Microbes\nDry Soil", "Wet Microbes\nWet Soil")
means_fn_dataframe <- data.frame(means_labels_vect,fn_means_vect)
dev.new(width=8,height=6)
par(lwd=3, cex=1.2)
barx <- barplot(means_fn_dataframe$fn_means_vect, names.arg=means_fn_dataframe$means_labels_vect, ylab=list("Fruit Number",cex=1.1), xlab=list("Treatment Combination",cex=1.1), col=c("firebrick","turquoise","firebrick","turquoise"), space=c(0,0,0.2,0), ylim=c(0,6))
axis(2,lwd=3)
arrows(barx[1],fn_means[1]+fn_sem[1],barx[1],fn_means[1]-fn_sem[1], angle=90,code=3)
arrows(barx[2],fn_means[2]+fn_sem[2],barx[2],fn_means[2]-fn_sem[2], angle=90,code=3)
arrows(barx[3],fn_means[3]+fn_sem[3],barx[3],fn_means[3]-fn_sem[3], angle=90,code=3)
arrows(barx[4],fn_means[4]+fn_sem[4],barx[4],fn_means[4]-fn_sem[4], angle=90,code=3)

#ANOVA
logfn_aov <- aov(nona_fruit_data$log_fn ~ nona_fruit_data$Microbe_History + nona_fruit_data$Contemp_Water + nona_fruit_data$Microbe_History*nona_fruit_data$Contemp_Water)
anova(logfn_aov)

#The anova indicates significant effects of microbe history, contemporary water, and the interaction

posthoc <- TukeyHSD(logfn_aov)
posthoc

#The post-hoc test indicates that all of the treatments differ from one another, except Wet Microbes on Wet Soil compared to Dry Microbes on Wet Soil.
#This makes sense if you look at the graphs, and is probably partly caused by the large standard error in the Wet Microbes-Wet treatment.


####### Flower Number #######

nona_flowernum_data <- less_plant_data[c(1,2,4)] # Reduce the data again to just include flower number as a response
nona_flowernum_data <- subset(nona_flowernum_data, Flower_number > -1) # Remove rows with "NA"
hist(nona_flowernum_data$Flower_number) #just a quick peek at the histogram to see how goofed up it is -- Okay, it's bad.
nona_flowernum_data$log_flownum <- log(nona_flowernum_data$Flower_number + 1) # add a column with ln flower number
hist(nona_flowernum_data$log_flownum) #The log transformed data look way better.

#Note that technically, you should be looking at the histogram within each group. However, if the overall histogram is normal, then the histograms within each
#group are probably normal. Similarly, if the overall histogram has a huge excess of low values, then the histograms within each group probably are not normal.
#If you see something else, like a multimodal distribution, you should check within each treatment, like so:

wetwetFN <- subset(nona_flowernum_data, Microbe_History == "Wet microbes" & Contemp_Water == "Wet")
hist(wetwetFN$Flower_number)
drywetFN <- subset(nona_flowernum_data, Microbe_History == "Dry microbes" & Contemp_Water == "Wet")
hist(drywetFN$Flower_number)
wetdryFN <- subset(nona_flowernum_data, Microbe_History == "Wet microbes" & Contemp_Water == "Dry")
hist(wetdryFN$Flower_number)
drydryFN <- subset(nona_flowernum_data, Microbe_History == "Dry microbes" & Contemp_Water == "Dry")
hist(drydryFN$Flower_number)

#Now the log transformed values:
hist(wetwetFN$log_flownum)
hist(drywetFN$log_flownum)
hist(wetdryFN$log_flownum)
hist(drydryFN$log_flownum)

# Calculate mean and sd
flownum_means <- tapply(nona_flowernum_data$Flower_number,INDEX=list(nona_flowernum_data$Contemp_Water,nona_flowernum_data$Microbe_History),mean)
flownum_stdev <- tapply(nona_flowernum_data$Flower_number,INDEX=list(nona_flowernum_data$Contemp_Water,nona_flowernum_data$Microbe_History),sd)
flownum_length <- tapply(nona_flowernum_data$Flower_number,INDEX=list(nona_flowernum_data$Contemp_Water,nona_flowernum_data$Microbe_History),length)
flownum_sem <- flownum_stdev/sqrt(flownum_length)
flownum_means
flownum_sem

flownum_means_vect <- flownum_means[1:4]
means_labels_vect = c("Dry Microbes\nDry Soil","Dry Microbes\nWet Soil", "Wet Microbes\nDry Soil", "Wet Microbes\nWet Soil")
means_flownum_dataframe <- data.frame(means_labels_vect,flownum_means_vect)
dev.new(width=8,height=6)
par(lwd=3, cex=1.2)
barx <- barplot(means_flownum_dataframe$flownum_means_vect, names.arg=means_flownum_dataframe$means_labels_vect, ylab=list("Flower Number",cex=1.1), xlab=list("Treatment Combination",cex=1.1), col=c("firebrick","turquoise","firebrick","turquoise"), space=c(0,0,0.2,0), ylim=c(0,35))
axis(2,lwd=3)
arrows(barx[1],flownum_means[1]+flownum_sem[1],barx[1],flownum_means[1]-flownum_sem[1], angle=90,code=3)
arrows(barx[2],flownum_means[2]+flownum_sem[2],barx[2],flownum_means[2]-flownum_sem[2], angle=90,code=3)
arrows(barx[3],flownum_means[3]+flownum_sem[3],barx[3],flownum_means[3]-flownum_sem[3], angle=90,code=3)
arrows(barx[4],flownum_means[4]+flownum_sem[4],barx[4],flownum_means[4]-flownum_sem[4], angle=90,code=3)

#ANOVA
logflownum_aov <- aov(nona_flowernum_data$log_flownum ~ nona_flowernum_data$Microbe_History*nona_flowernum_data$Contemp_Water) #note that if you put the interaction term, it will automatically test the main effects
anova(logflownum_aov)

#The anova indicates significant effects of microbe history, contemporary water, and the interaction (much like the last one)

posthoc <- TukeyHSD(logflownum_aov)
posthoc

#For flower number, Wet Microbes on Wet Soil are different from all the rest. The other three are not significantly different from one another.

######## Days to Flower #######

nona_dtf_data <- less_plant_data[c(1,2,5)] # Reduce the data again to just include days to flower as a response
nona_dtf_data <- subset(nona_dtf_data, Days_to_flower > -1) # Remove rows with "NA"
hist(nona_dtf_data$Days_to_flower) 
nona_dtf_data$log_dtf <- log(nona_dtf_data$Days_to_flower) # add a column with ln days to flower
hist(nona_dtf_data$log_dtf) 
#sqrt transformation
nona_dtf_data$sqrtdtf <- sqrt(nona_dtf_data$Days_to_flower)
hist(nona_dtf_data$sqrtdtf)

#Check histograms within each treatment
wetwetDTF <- subset(nona_dtf_data, Microbe_History == "Wet microbes" & Contemp_Water == "Wet")
hist(wetwetDTF$Days_to_flower)
drywetDTF <- subset(nona_dtf_data, Microbe_History == "Dry microbes" & Contemp_Water == "Wet")
hist(drywetDTF$Days_to_flower)
wetdryDTF <- subset(nona_dtf_data, Microbe_History == "Wet microbes" & Contemp_Water == "Dry")
hist(wetdryDTF$Days_to_flower)
drydryDTF <- subset(nona_dtf_data, Microbe_History == "Dry microbes" & Contemp_Water == "Dry")
hist(drydryDTF$Days_to_flower)

#Now the log transformed values:
hist(wetwetDTF$log_dtf)
hist(drywetDTF$log_dtf)
hist(wetdryDTF$log_dtf)
hist(drydryDTF$log_dtf)

#Now the sqrt transformed
hist(wetwetDTF$sqrtdtf)
hist(drywetDTF$sqrtdtf)
hist(wetdryDTF$sqrtdtf)
hist(drydryDTF$sqrtdtf)

#Overall, I think the log transformation is the best even though no transformation achieves strict normality

# Calculate mean and sd
dtf_means <- tapply(nona_dtf_data$Days_to_flower,INDEX=list(nona_dtf_data$Contemp_Water,nona_dtf_data$Microbe_History),mean)
dtf_stdev <- tapply(nona_dtf_data$Days_to_flower,INDEX=list(nona_dtf_data$Contemp_Water,nona_dtf_data$Microbe_History),sd)
dtf_length <- tapply(nona_dtf_data$Days_to_flower,INDEX=list(nona_dtf_data$Contemp_Water,nona_dtf_data$Microbe_History),length)
dtf_sem <- dtf_stdev/sqrt(dtf_length)
dtf_means
dtf_sem

dtf_means_vect <- dtf_means[1:4]
means_dtf_vect = c("Dry Microbes\nDry Soil","Dry Microbes\nWet Soil", "Wet Microbes\nDry Soil", "Wet Microbes\nWet Soil")
means_dtf_dataframe <- data.frame(means_labels_vect,dtf_means_vect)
dev.new(width=8,height=6)
par(lwd=3, cex=1.2)
barx <- barplot(means_dtf_dataframe$dtf_means_vect, names.arg=means_dtf_dataframe$means_labels_vect, ylab=list("Days to Flower",cex=1.1), xlab=list("Treatment Combination",cex=1.1), col=c("firebrick","turquoise","firebrick","turquoise"), space=c(0,0,0.2,0), ylim=c(0,35))
axis(2,lwd=3)
arrows(barx[1],dtf_means[1]+dtf_sem[1],barx[1],dtf_means[1]-dtf_sem[1], angle=90,code=3)
arrows(barx[2],dtf_means[2]+dtf_sem[2],barx[2],dtf_means[2]-dtf_sem[2], angle=90,code=3)
arrows(barx[3],dtf_means[3]+dtf_sem[3],barx[3],dtf_means[3]-dtf_sem[3], angle=90,code=3)
arrows(barx[4],dtf_means[4]+dtf_sem[4],barx[4],dtf_means[4]-dtf_sem[4], angle=90,code=3)

#ANOVA
logdtf_aov <- aov(nona_dtf_data$log_dtf ~ nona_dtf_data$Microbe_History*nona_dtf_data$Contemp_Water) #note that if you put the interaction term, it will automatically test the main effects
anova(logdtf_aov)

#The anova indicates significant effects of microbe history and contemporary water, but no interaction -- more water decreases days to flower, dry microbes alos decrease days to flower.

posthoc <- TukeyHSD(logdtf_aov)
posthoc

#Owing to the very large sample sizes, all of the bars are different from one another, except that Dry Microbes:Wet is not different from Dry microbes:Dry.

