#!/usr/bin/env Rscript

##############################################################
############## IMPORT DATA AND CREATE PLOTS ##################
##############################################################

##### LIBRARIES #####

library(ggplot2)
library(reshape2)
library(plyr)
library(stringr)
library(moments)

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
aa <- matrix(data=NA, nrow=201, ncol=20)
dd <- matrix(data=NA, nrow=201, ncol=20)
gg <- matrix(data=NA, nrow=201, ncol=20)
jj <- matrix(data=NA, nrow=201, ncol=20)
#mm <- matrix(data=NA, nrow=201, ncol=20)


# import file then perform sd and put into a, move to next file and next column, etc.
for (j in 1:20) {
	filename <- paste("10mil_gens/brownian_", j, ".txt", sep="")
	datafile <- read.table(file=filename,row.names=1)
	for (i in 1:201){
		print(paste("processing generation", i*time[2]-time[2], "of", filename))
		aa[i,j] <- apply(datafile[i,],1,var)
		dd[i,j] <- apply(datafile[i,],1,mean)
		gg[i,j] <- apply(datafile[i,],1,kurtosis)   # Pearson's calculation
		jj[i,j] <- apply(datafile[i,],1,skewness)
#		temp <- apply(datafile[i,],1,jarque.test)
#		mm[i,j] <- temp$p.value
		if (i==201){
			print("file complete")
		}
	}
}

bb <- matrix(data=NA, nrow=201, ncol=20)
ee <- matrix(data=NA, nrow=201, ncol=20)
hh <- matrix(data=NA, nrow=201, ncol=20)
kk <- matrix(data=NA, nrow=201, ncol=20)
#nn <- matrix(data=NA, nrow=201, ncol=20)

for (j in 1:20) {
	filename <- paste("10mil_gens/single_bottleneck_", j, ".txt", sep="")
	datafile <- read.table(file=filename,row.names=1)
	for (i in 1:201){
		print(paste("processing generation", i*time[2]-time[2], "of", filename))
		bb[i,j] <- apply(datafile[i,],1,var)
		ee[i,j] <- apply(datafile[i,],1,mean)
		hh[i,j] <- apply(datafile[i,],1,kurtosis)
		kk[i,j] <- apply(datafile[i,],1,skewness)
#		temp <- apply(datafile[i,],1,jarque.test)
#		nn[i,j] <- temp$p.value
		if (i==201){
			print("file complete")
		}
	}
}

cc <- matrix(data=NA, nrow=201, ncol=20)
ff <- matrix(data=NA, nrow=201, ncol=20)
ii <- matrix(data=NA, nrow=201, ncol=20)
ll <- matrix(data=NA, nrow=201, ncol=20)
oo <- matrix(data=NA, nrow=201, ncol=20)

for (j in 1:20) {
	filename <- paste("10mil_gens/multi_bottleneck_", j, ".txt", sep="")
	datafile <- read.table(file=filename,row.names=1)
	for (i in 1:201){
		print(paste("processing generation", i*time[2]-time[2], "of", filename))
		cc[i,j] <- apply(datafile[i,],1,var)
		ff[i,j] <- apply(datafile[i,],1,mean)
		ii[i,j] <- apply(datafile[i,],1,kurtosis)
		ll[i,j] <- apply(datafile[i,],1,skewness)
#		temp <- apply(datafile[i,],1,jarque.test)
#		oo[i,j] <- temp$p.value
		if (i==201){
			print("file complete")
		}
	}
}

##### DATA FRAME #####

brownian.var <- as.data.frame(aa)
single.var <- as.data.frame(bb)
multi.var <- as.data.frame(cc)

brownian.mean <- as.data.frame(dd)
single.mean <- as.data.frame(ee)
multi.mean <- as.data.frame(ff)

brownian.kurtosis <- as.data.frame(gg)
single.kurtosis <- as.data.frame(hh)
multi.kurtosis <- as.data.frame(ii)

brownian.skewness <- as.data.frame(jj)
single.skewness <- as.data.frame(kk)
multi.skewness <- as.data.frame(ll)

#brownian.jarq_pval <- as.data.frame(mm)
#single.jarq_pval <- as.data.frame(nn)
#multi.jarq_pval <- as.data.frame(oo)

#fix initial kurtosis value#
brownian.kurtosis[1,] <- -1
single.kurtosis[1,] <- -1
multi.kurtosis[1,] <- -1

##### CALCULATE MEANS OF SDS #####

# mean for each time step #
v.mean.b <- apply(brownian.var, 1, mean)
v.mean.s <- apply(single.var, 1, mean)
v.mean.m <- apply(multi.var, 1, mean)

m.mean.b <- apply(brownian.mean, 1, mean)
m.mean.s <- apply(single.mean, 1, mean)
m.mean.m <- apply(multi.mean, 1, mean)

k.mean.b <- apply(brownian.kurtosis, 1, mean)
k.mean.s <- apply(single.kurtosis, 1, mean)
k.mean.m <- apply(multi.kurtosis, 1, mean)

#j.mean.b <- apply(brownian.jarq_pval, 1, mean)
#j.mean.s <- apply(single.jarq_pval, 1, mean)
#j.mean.m <- apply(multi.jarq_pval, 1, mean)

#################################################################
##### SINGLE BOTTLENECK v. NO BOTTLENECK CORRECTED MEAN VAR #####
#################################################################

##### CALCULATE P-VALUES FOR EACH DATAPOINT #####

pval.s.v <- as.vector(matrix(data = NA, nrow = 201, ncol = 1))

for (i in 1:201){
	temp <- t.test(brownian.var[i,], single.var[i,])
	pval.s.v[i] <- temp$p.value
}

# replace NA b/c t-test between identical distributions
pval.s.v[1] <- 1    

p.value <- pval.s.v

##### GRAPH #####

sd.diff <- v.mean.b - v.mean.s
trait.comb <- cbind(v.mean.b, v.mean.s)
mean.traits <- apply(trait.comb, 1, mean)
sd.corrected <- sd.diff/mean.traits
sd.corrected[1] <- 0      # replace NA b/c can't divide by 0
sd.diff.graph <- cbind(time, sd.corrected, p.value)

sd.diff.df <- as.data.frame(sd.diff.graph)

ggplot(data=sd.diff.df, aes(x=time, y=sd.corrected)) + geom_line(aes(color=-p.value)) + geom_point(aes(color=-p.value, size=-p.value)) + geom_hline(yintercept=0, color="orange", linetype=2) + scale_x_continuous("time (in generations)") + scale_y_continuous("corrected change in var") + geom_vline(xintercept=c(bottleneck.single*realtime), color="red")
ggsave(filename="single_v_noBN_final_VAR.pdf", height=5.5, width=8)

###############################################################
##### MULTIBOTTLENECK v. NO BOTTLENECK CORRECTED MEAN VAR #####
###############################################################

##### CALCULATE P-VALUES FOR EACH DATAPOINT #####

pval.m.v <- as.vector(matrix(data = NA, nrow = 201, ncol = 1))

for (i in 1:201){
	temp <- t.test(brownian.var[i,], multi.var[i,])
	pval.m.v[i] <- temp$p.value
}

# replace NA b/c t-test between identical distributions
pval.m.v[1] <- 1    

p.value <- pval.m.v

##### GRAPH #####

sd.diff <- v.mean.b - v.mean.m
trait.comb <- cbind(v.mean.b, v.mean.m)
mean.traits <- apply(trait.comb, 1, mean)
sd.corrected <- sd.diff/mean.traits
sd.corrected[1] <- 0      # replace NA b/c can't divide by 0
sd.diff.graph <- cbind(time, sd.corrected, p.value)

sd.diff.df <- as.data.frame(sd.diff.graph)

ggplot(data=sd.diff.df, aes(x=time, y=sd.corrected)) + geom_line(aes(color=-p.value)) + geom_point(aes(color=-p.value, size=-p.value)) + geom_hline(yintercept=0, color="orange", linetype=2) + scale_x_continuous("time (in generations)") + scale_y_continuous("corrected change in var") + geom_vline(xintercept=c(bottleneck.multi*realtime), color="red")
ggsave(filename="multi_v_noBN_final_VAR.pdf", height=5.5, width=8)

#######################################################################
##### SINGLE BOTTLENECK v. NO BOTTLENECK DIFFERENCE MEAN KURTOSIS #####
#######################################################################

##### CALCULATE P-VALUES FOR EACH DATAPOINT #####

pval.s.k <- as.vector(matrix(data = NA, nrow = 201, ncol = 1))

for (i in 1:201){
	if (i == 1){
		pval.s.k[i] <-        1   # replace NaN b/c t-test fails
	}
	else {
		temp <- t.test(brownian.kurtosis[i,], single.kurtosis[i,])
		pval.s.k[i] <- temp$p.value
	}
}

p.value <- pval.s.k

##### GRAPH #####

k1.diff <- k.mean.b - k.mean.s
k1.diff.graph <- cbind(time, k1.diff, p.value)

k1.diff.df <- as.data.frame(k1.diff.graph)

ggplot(data=k1.diff.df, aes(x=time, y=k1.diff)) + geom_line(aes(color=-p.value)) + geom_point(aes(color=-p.value, size=-p.value)) + geom_hline(yintercept=0, color="orange", linetype=2) + scale_x_continuous("time (in generations)") + scale_y_continuous("difference in kurtosis") + geom_vline(xintercept=c(bottleneck.single*realtime), color="red")
ggsave(filename="single_v_noBN_final_KUR.pdf", height=5.5, width=8)

########################################################################
##### MULTIBOTTLENECK v. NO BOTTLENECK DIFFERENCE IN MEAN KURTOSIS #####
########################################################################

##### CALCULATE P-VALUES FOR EACH DATAPOINT #####

pval.m.k <- as.vector(matrix(data = NA, nrow = 201, ncol = 1))

for (i in 1:201){
	if (i == 1){
		pval.m.k[i] <- 1               # replace NaN b/c t-test fails
	}
	else {
		temp <- t.test(brownian.kurtosis[i,], multi.kurtosis[i,])
		pval.m.k[i] <- temp$p.value
	}
}

p.value <- pval.m.k

##### GRAPH #####

k2.diff <- k.mean.b - k.mean.m
k2.diff.graph <- cbind(time, k2.diff, p.value)

k2.diff.df <- as.data.frame(k2.diff.graph)

ggplot(data=k2.diff.df, aes(x=time, y=k2.diff)) + geom_line(aes(color=-p.value)) + geom_point(aes(color=-p.value, size=-p.value)) + geom_hline(yintercept=0, color="orange", linetype=2) + scale_x_continuous("time (in generations)") + scale_y_continuous("difference in kurtosis") + geom_vline(xintercept=c(bottleneck.multi*realtime), color="red")
ggsave(filename="multi_v_noBN_final_KUR.pdf", height=5.5, width=8)


####################################################################
##### RIBBON PLOT OF SINGLE v. NO BOTTLNECK MEAN AND VARIANCES #####
####################################################################

bind <- cbind(time, m.mean.b, m.mean.s) 
comp <- as.data.frame(bind)

b.ymin <- m.mean.b-2*sqrt(v.mean.b)
b.ymax <- m.mean.b+2*sqrt(v.mean.b)
s.ymin <- m.mean.s-2*sqrt(v.mean.s)
s.ymax <- m.mean.s+2*sqrt(v.mean.s)

ggplot(data=comp, aes(time)) + geom_ribbon(aes(ymin=b.ymin, ymax=b.ymax), color="red", fill="red", alpha = 0.5) + geom_ribbon(aes(ymin=s.ymin, ymax=s.ymax), color="blue", fill="blue", alpha = 0.5) + geom_line(aes(y=m.mean.b), color="red") + geom_line(aes(y=m.mean.s), color="blue") + scale_x_continuous("time (in generations)") + scale_y_continuous("trait value")  + geom_vline(xintercept=bottleneck.single*realtime, color="black")
ggsave(filename="ribbonplotofmeansNOvSING.pdf", height=5.5, width=8)

# closeup

closeup <- comp[1:40,]

c.b.ymin <- b.ymin[1:40]
c.b.ymax <- b.ymax[1:40]
c.s.ymin <- s.ymin[1:40]
c.s.ymax <- s.ymax[1:40]

ggplot(data=closeup, aes(time)) + geom_ribbon(aes(ymin=c.b.ymin, ymax=c.b.ymax), color="red", fill="red", alpha = 0.5) + geom_ribbon(aes(ymin=c.s.ymin, ymax=c.s.ymax), color="blue", fill="blue", alpha = 0.5) + geom_line(aes(y=m.mean.b), color="red") + geom_line(aes(y=m.mean.s), color="blue") + scale_x_continuous("time (in generations)") + scale_y_continuous("trait value")  + geom_vline(xintercept=bottleneck.single*realtime, color="black")
ggsave(filename="ribbonplotcloseupNOvSING.pdf", height=5.5, width=8)

####################################################################
##### RIBBON PLOT OF MULTI v. NO BOTTLNECK MEAN AND VARIANCES #####
####################################################################

bind2 <- cbind(time, m.mean.b, m.mean.m) 
comp2 <- as.data.frame(bind)

b.ymin <- m.mean.b-2*sqrt(v.mean.b)
b.ymax <- m.mean.b+2*sqrt(v.mean.b)
m.ymin <- m.mean.m-2*sqrt(v.mean.m)
m.ymax <- m.mean.m+2*sqrt(v.mean.m)

ggplot(data=comp2, aes(time)) + geom_ribbon(aes(ymin=b.ymin, ymax=b.ymax), color="red", fill="red", alpha = 0.5) + geom_ribbon(aes(ymin=m.ymin, ymax=m.ymax), color="blue", fill="blue", alpha = 0.5) + geom_line(aes(y=m.mean.b), color="red") + geom_line(aes(y=m.mean.m), color="blue") + scale_x_continuous("time (in generations)") + scale_y_continuous("trait value")  + geom_vline(xintercept=bottleneck.multi*realtime, color="black")
ggsave(filename="ribbonplotofmeansNOvMULT.pdf", height=5.5, width=8)

# closeup

closeup2 <- comp[1:100,]

c.b.ymin <- b.ymin[1:100]
c.b.ymax <- b.ymax[1:100]
c.m.ymin <- m.ymin[1:100]
c.m.ymax <- m.ymax[1:100]

ggplot(data=closeup2, aes(time)) + geom_ribbon(aes(ymin=c.b.ymin, ymax=c.b.ymax), color="red", fill="red", alpha = 0.5) + geom_ribbon(aes(ymin=c.m.ymin, ymax=c.m.ymax), color="blue", fill="blue", alpha = 0.5) + geom_line(aes(y=m.mean.b), color="red") + geom_line(aes(y=m.mean.s), color="blue") + scale_x_continuous("time (in generations)") + scale_y_continuous("trait value")  + geom_vline(xintercept=bottleneck.multi*realtime, color="black")
ggsave(filename="ribbonplotcloseupNOvMULT.pdf", height=5.5, width=8)


##### SKEW OVER TIME #####

##### KURTOSIS OVER TIME #####

