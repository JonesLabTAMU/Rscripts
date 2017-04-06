## Using a repeated measures ANOVA to analyze bee gene expression data

setwd("Rexamples/Week08")
holman <- read.csv("Holman_data_for_exercise.csv")

# Species doesn't matter, because there's only one
# There are multiple colonies, so Colony should probably be in the model
# Obviously Treatment is important
# Each bee was measured twice -- once for each gene, so there are repeated 
# measures within subjects. Consequently, SubjectID is important
# It seems like PlateID should matter, but it turns out to be completely
# confounded with Gene, so there's nothing to do about that.
# The final column that matters is g, which is the gene expression value.

# Check assumptions of the model

# Is the design balanced? Yes, it is.

# Sphericity: Do the differences between repeated measure columns have equal
# variances? Since there are only two repeated measures per subject, this issue
# doesn't arise. If there were three genes, then it might be important to 
# examine this assumption (i.e., gene1-gene2, gene2-gene3, gene1-gene3).

# Normality: Is the response variable normally distributed within each treatment
# category? It might be worth looking at the overall distributions, and the
# the distributions by Treatment, Gene and Colony.

# The quickest and easiest way is to use lattice

library(lattice)
histogram(~g | Gene, data=holman)
histogram(~g | Gene*Treatment, data=holman)
histogram(~g | Gene*Colony, data=holman)

# Nothing about the distributions causes alarm

# Specifying the model:
# Colony is a blocking variable (random factor), 
# so it should come first and have no interactions.
# Gene and treatment are fixed effects, and we care about the interaction.
# Gene measurments are within-individuals (i.e., each individual was measured for each gene)

holman_model <- holman_anova <- aov(g ~ Colony + Treatment*Gene + Error(SampleID/Gene), data=holman)
summary(holman_anova)

# The results show that Colonies are different. More importantly, the treatment does have
# an effect, and the Genes have different expression levels. There's no interaction, so
# the treatment affected both genes in the same way.

# Show the data as a boxplot
par(cex=1.2, lwd=2)
boxplot(g ~ Treatment+Gene, data=holman, ylab="Gene Expression Value", xlab="Treatment and Gene", names=c("\nControl\nDNMT1","\nExperimental\nDNMT1","\nControl\nDNMT3","\nExperimental\nDNMT3"))
box(lwd=3)




