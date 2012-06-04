#!/usr/bin/env Rscript
# IMPORT DATA AND CREATE PLOTS #

##### SETTINGS #####

realtime <- 5000000
points <- 201
time <- as.vector(matrix(data=NA, nrow=200, ncol=1))

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

a <- matrix(data=NA, nrow=21, ncol=40)

for (j in 1:20) {
	filename <- paste("10mil_gens/brownian_", j, ".txt", sep="")
	datafile <- read.table(file=filename,row.names=1)
	for (i in 1:21){
		a[i,j] <- apply(datafile[i,],1,sd)
		print(paste("processing generation", i*50000-50000, "of", filename))
		if (i==21){
			print("file complete")
		}
	}
}

b <- matrix(data=NA, nrow=21, ncol=40)

for (j in 1:20) {
	filename <- paste("brownian_replicates_2/bottleneck_", j, ".txt", sep="")
	datafile <- read.table(file=filename,row.names=1)
	for (i in 1:21){
		b[i,j] <- apply(datafile[i,],1,sd)
		print(paste("processing generation", i*50000-50000, "of", filename))
		if (i==21){
			print("file complete")
		}
	}
}

c <- matrix(data=NA, nrow=21, ncol=40)

for (j in 1:20) {
	filename <- paste("brownian_replicates_2/bottleneck_", j, ".txt", sep="")
	datafile <- read.table(file=filename,row.names=1)
	for (i in 1:21){
		c[i,j] <- apply(datafile[i,],1,sd)
		print(paste("processing generation", i*50000-50000, "of", filename))
		if (i==21){
			print("file complete")
		}
	}
}

