#!/usr/bin/env Rscript

##############################################################
############## IMPORT DATA AND CREATE PLOTS ##################
##############################################################

##### LIBRARIES #####

library(ggplot2)
library(reshape2)
library(plyr)
library(stringr)

##### SETTINGS #####

realtime <- 5000000
points <- 201
time <- as.vector(matrix(data=NA, nrow=201, ncol=1))

for (i in 1:points){
	time[i] <- ((i-1)/(points-1))*realtime
}

##### OTHER SETTINGS #####

bottleneck.multi <- c(0.1, 0.15, 0.2, 0.25, 0.3)
bottleneck.single <- 0.1
bottleneck.proportion <- 0.01
loci <- 1000
mutation.rate <- 4 * 10^-6
individuals.lineages <- 1000

##### IMPORT DATA ######
# make sure to fix size of matricies for number of files#

# create matrix
a <- matrix(data=NA, nrow=201, ncol=20)

# import file then perform sd and put into a, move to next file and next column, etc.
for (j in 1:20) {
	filename <- paste("10mil_gens/brownian_", j, ".txt", sep="")
	datafile <- read.table(file=filename,row.names=1)
	for (i in 1:201){
		a[i,j] <- apply(datafile[i,],1,sd)
		print(paste("processing generation", i*time[2]-time[2], "of", filename))
		if (i==201){
			print("file complete")
		}
	}
}

b <- matrix(data=NA, nrow=201, ncol=20)

for (j in 1:20) {
	filename <- paste("10mil_gens/single_bottleneck_", j, ".txt", sep="")
	datafile <- read.table(file=filename,row.names=1)
	for (i in 1:201){
		b[i,j] <- apply(datafile[i,],1,sd)
		print(paste("processing generation", i*time[2]-time[2], "of", filename))
		if (i==201){
			print("file complete")
		}
	}
}

c <- matrix(data=NA, nrow=201, ncol=20)

for (j in 1:20) {
	filename <- paste("10mil_gens/multi_bottleneck_", j, ".txt", sep="")
	datafile <- read.table(file=filename,row.names=1)
	for (i in 1:201){
		c[i,j] <- apply(datafile[i,],1,sd)
		print(paste("processing generation", i*time[2]-time[2], "of", filename))
		if (i==201){
			print("file complete")
		}
	}
}

##### DATA FRAME #####

brownian <- as.data.frame(a)
single <- as.data.frame(b)
multi <- as.data.frame(c)

##### CALCULATE MEANS OF SDS #####

# mean for each time step #
mean.b <- apply(brownian, 1, mean)
mean.s <- apply(single, 1, mean)
mean.m <- apply(multi, 1, mean)

################################################################
##### SINGLE BOTTLENECK v. NO BOTTLENECK CORRECTED MEAN SD #####
################################################################

##### CALCULATE P-VALUES FOR EACH DATAPOINT #####

pval <- as.vector(matrix(data = NA, nrow = 201, ncol = 1))

for (i in 1:201){
	temp <- t.test(brownian[i,], single[i,])
	pval[i] <- temp$p.value
}

# replace NA b/c t-test between identical distributions
pval[1] <- 1    

p.value <- pval

##### GRAPH #####

sd.diff <- mean.b - mean.s
trait.comb <- cbind(mean.b, mean.s)
mean.traits <- apply(trait.comb, 1, mean)
sd.corrected <- sd.diff/mean.traits
sd.corrected[1] <- 0      # replace NA b/c can't divide by 0
sd.diff.graph <- cbind(time, sd.corrected, p.value)

sd.diff.df <- as.data.frame(sd.diff.graph)

ggplot(data=sd.diff.df, aes(x=time, y=sd.corrected)) + geom_line(aes(color=-p.value)) + geom_point(aes(color=-p.value, size=-p.value)) + geom_hline(yintercept=0, color="orange", linetype=2) + scale_x_continuous("time (in generations)") + scale_y_continuous("corrected change in sd") + geom_vline(xintercept=c(bottleneck.single*realtime), color="red")
ggsave(filename="single_v_noBN_final.pdf")

##############################################################
##### MULTIBOTTLENECK v. NO BOTTLENECK CORRECTED MEAN SD #####
##############################################################

##### CALCULATE P-VALUES FOR EACH DATAPOINT #####

pval <- as.vector(matrix(data = NA, nrow = 201, ncol = 1))

for (i in 1:201){
	temp <- t.test(brownian[i,], multi[i,])
	pval[i] <- temp$p.value
}

# replace NA b/c t-test between identical distributions
pval[1] <- 1    

p.value <- pval

##### GRAPH #####

sd.diff <- mean.b - mean.m
trait.comb <- cbind(mean.b, mean.m)
mean.traits <- apply(trait.comb, 1, mean)
sd.corrected <- sd.diff/mean.traits
sd.corrected[1] <- 0      # replace NA b/c can't divide by 0
sd.diff.graph <- cbind(time, sd.corrected, p.value)

sd.diff.df <- as.data.frame(sd.diff.graph)

ggplot(data=sd.diff.df, aes(x=time, y=sd.corrected)) + geom_line(aes(color=-p.value)) + geom_point(aes(color=-p.value, size=-p.value)) + geom_hline(yintercept=0, color="orange", linetype=2) + scale_x_continuous("time (in generations)") + scale_y_continuous("corrected change in sd") + geom_vline(xintercept=c(bottleneck.multi*realtime), color="red")
ggsave(filename="multi_v_noBN_final.pdf")

############################################
##### RIBBON PLOT MEAN DATA COLLECTION #####
############################################

a <- matrix(data=NA, nrow=201, ncol=20)

# import file then perform sd and put into a, move to next file and next column, etc.
for (j in 1:20) {
	filename <- paste("10mil_gens/brownian_", j, ".txt", sep="")
	datafile <- read.table(file=filename,row.names=1)
	for (i in 1:201){
		a[i,j] <- apply(datafile[i,],1,sd)
		print(paste("processing generation", i*time[2]-time[2], "of", filename))
		if (i==201){
			print("file complete")
		}
	}
}

b <- matrix(data=NA, nrow=201, ncol=20)

for (j in 1:20) {
	filename <- paste("10mil_gens/single_bottleneck_", j, ".txt", sep="")
	datafile <- read.table(file=filename,row.names=1)
	for (i in 1:201){
		b[i,j] <- apply(datafile[i,],1,sd)
		print(paste("processing generation", i*time[2]-time[2], "of", filename))
		if (i==201){
			print("file complete")
		}
	}
}

c <- matrix(data=NA, nrow=201, ncol=20)

for (j in 1:20) {
	filename <- paste("10mil_gens/multi_bottleneck_", j, ".txt", sep="")
	datafile <- read.table(file=filename,row.names=1)
	for (i in 1:201){
		c[i,j] <- apply(datafile[i,],1,sd)
		print(paste("processing generation", i*time[2]-time[2], "of", filename))
		if (i==201){
			print("file complete")
		}
	}
}


#################################################
##### RIBBON PLOT OF SINGLE v. NO BOTTLNECK #####
#################################################

bind <- cbind(time, m., trait.mean2) 
#comp <- as.data.frame(bind)

#ggplot(data=comp, aes(time)) + geom_ribbon(aes(ymin=trait.mean1-2*trait.sd1, ymax=trait.mean1+2*trait.sd1), color="#FF0000", fill="#FF0000", alpha = 0.5) + geom_ribbon(aes(ymin=trait.mean2-2*trait.sd2, ymax=trait.mean2+2*trait.sd2), color="#0000FF", fill="#0000FF", alpha = 0.5) + geom_line(aes(y=trait.mean1), color="#FF0000") + geom_line(aes(y=trait.mean2), color="#0000FF") + scale_x_continuous("time (in generations)") + scale_y_continuous("mean sd (trait value)")  + geom_vline(xintercept=bottleneck*length(time), color="red")
#ggsave(filename=ribbonplotofmeanofsds.pdf)
