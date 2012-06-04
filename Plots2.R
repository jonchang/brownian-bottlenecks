#!/usr/bin/Rscript

##################### Trait evolution plots ###################################

### For practice -- simulated brownian motion trait data

### WHEN USING REAL DATA BE SURE TO CHANGE NAMES AND TIME VALUES TO APPROPRIATE NUMBER OF GENERATIONS ###

realtime <- 1000000
time <- c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 450000, 500000, 550000, 600000, 650000, 700000, 750000, 800000, 850000, 900000, 950000, 1000000)
bottleneck <- c(0.2)
loci <- 1000
individuals <- 1000

######### READ IN FILES AND CALC SD ###########################################

a <- matrix(data=NA, nrow=21, ncol=40)

for (j in 1:20) {
	filename <- paste("brownian_replicates/brownian_", j, ".txt", sep="")
	datafile <- read.table(file=filename,row.names=1)
	for (i in 1:21){
		a[i,j] <- apply(datafile[i,],1,sd)
		print(paste("processing generation", i*50000-50000, "of", filename))
		if (i==21){
			print("file complete")
		}
	}
}

for (j in 1:20) {
	filename <- paste("brownian_replicates_2/brownian_", j, ".txt", sep="")
	datafile <- read.table(file=filename,row.names=1)
	for (i in 1:21){
		a[i,j+20] <- apply(datafile[i,],1,sd)
		print(paste("processing generation", i*50000-50000, "of", filename))
		if (i==21){
			print("file complete")
		}
	}
}

b <- matrix(data=NA, nrow=21, ncol=20)

for (j in 1:20) {
	filename <- paste("single_bottleneck/bottleneck_", j, ".txt", sep="")
	datafile <- read.table(file=filename,row.names=1)
	for (i in 1:21){
		b[i,j] <- apply(datafile[i,],1,sd)
		print(paste("processing generation", i*50000-50000, "of", filename))
		if (i==21){
			print("file complete")
		}
	}
}

X.b <- a
X2 <- b

##############################################################################

#install.packages("ape")
library(ape)

### make data

#time_steps = 100000
#X.b = replicate(100, cumsum(c(0, rnorm(time_steps - 1))))

### change to data frame
### did not use "rownames(m2) <- rownames(m2, do.NULL = FALSE, prefix = "Obs.")" example because didn't work for colnames
### used instead string_concatenate

library(stringr)

df <- as.data.frame(X.b)
#colnames(df) <- str_c("ind.",1:100)       

### calculate summary data

trait.mean1 <- apply(df, 1, mean)
trait.sd1 <- apply(df, 1, sd) 

#install.packages("ggplot2")
library(ggplot2)
#install.packages("reshape2")
library(reshape2)
#library(plyr)

##################### GENERATE DATA SET 2 ##############################

#time_steps = 100000
#X2 = replicate(100, cumsum(c(0, rnorm(time_steps - 1))))

d2 <- as.data.frame(X2)
#colnames(d2) <- str_c("ind.",1:100)

trait.mean2 <- apply(d2, 1, mean)
trait.sd2 <- apply(d2, 1, sd)

####################### RIBBON PLOTS OF TWO DATASETS ####################

#bind <- cbind(time, trait.mean1, trait.mean2) 
#comp <- as.data.frame(bind)

#ggplot(data=comp, aes(time)) + geom_ribbon(aes(ymin=trait.mean1-2*trait.sd1, ymax=trait.mean1+2*trait.sd1), color="#FF0000", fill="#FF0000", alpha = 0.5) + geom_ribbon(aes(ymin=trait.mean2-2*trait.sd2, ymax=trait.mean2+2*trait.sd2), color="#0000FF", fill="#0000FF", alpha = 0.5) + geom_line(aes(y=trait.mean1), color="#FF0000") + geom_line(aes(y=trait.mean2), color="#0000FF") + scale_x_continuous("time (in generations)") + scale_y_continuous("mean sd (trait value)")  + geom_vline(xintercept=bottleneck*length(time), color="red")
#ggsave(filename=ribbonplotofmeanofsds.pdf)

#################### CALCULATE DIFF FROM MEAN #########################

### full set of differences (for future multiple t-tests)

#diff2 <- d2-trait.mean2
#diff1 <- df-trait.mean1

################## GRAPH MEAN CHANGE IN DIFF V. TIME ##################

#diff.mean1 <- apply(diff1, 1, mean)
#diff.mean2 <- apply(diff2, 1, mean)
#diff.mean.change <- diff.mean1-diff.mean2
#dmc.g <- cbind(time, diff.mean.change)

#dmc.df <- as.data.frame(dmc.g)

#ggplot(data=dmc.df, aes(x=time, y=diff.mean.change)) + geom_line() + geom_hline(yintercept=0, color="orange", linetype=2) + scale_x_continuous("time (in generations)") + scale_y_continuous("difference of mean difference") + geom_vline(xintercept=bottleneck*length(time), color="red")
#ggsave(filename=meanchange.pdf)

################## GRAPH BOTH SDS V. TIME #############################

#sds <- cbind(time, trait.sd1, trait.sd2)
#sd.compare <- melt(as.data.frame(sds), id.vars="time")

#ggplot(data=sd.compare, aes(x=time, y=value, color=variable)) + geom_line()  + geom_vline(xintercept=bottleneck*length(time), color="red")

################# CALCULATE P-VALUES OVER ALL GENERATIONS #############

pval <- matrix(data = NA, nrow = 21, ncol = 1)

for (i in 1:21){
	temp <- t.test(a[i,], b[i,])
	pval[i] <- temp$p.value
}

pval[1] <- 1       # replace NA b/c couldn't perform T-test on exact same distributions
pval <- as.vector(pval)        # change matrix to vector

################# GRAPH CORRECTED CHANGE IN SD V. TIME with P-VALUE LEGEND ###############

#sd.diff <- trait.sd2 - trait.sd1
#trait.comb <- cbind(trait.sd1, trait.sd2)
sd.diff <- trait.mean2 - trait.mean1
trait.comb <- cbind(trait.mean1, trait.mean2)
mean.traits <- apply(trait.comb, 1, mean)
sd.corrected <- sd.diff/mean.traits
sd.corrected[1] <- 0      # replace NA b/c can't divide by 0
sd.diff.graph <- cbind(time, sd.corrected, pval)

sd.diff.df <- as.data.frame(sd.diff.graph)

ggplot(data=sd.diff.df, aes(x=time, y=sd.corrected)) + geom_line(aes(color=-pval)) + geom_point(aes(color=-pval, size=-pval)) + geom_hline(yintercept=0, color="orange", linetype=2) + scale_x_continuous("time (in generations)") + scale_y_continuous("corrected change in sd") + geom_vline(xintercept=c(200000), color="red")
ggsave(filename="fuckingdone.pdf")


