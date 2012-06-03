#!/usr/bin/Rscript

##################### Trait evolution plots ############################

### For practice -- simulated browninan motion trait data

#install.packages("ape")
library(ape)

### make data

time_steps = 100000
X.b = replicate(100, cumsum(c(0, rnorm(time_steps - 1))))

### change to data frame
### did not use "rownames(m2) <- rownames(m2, do.NULL = FALSE, prefix = "Obs.")" example because didn't work for colnames
### used instead string_concatenate

library(stringr)

df <- as.data.frame(X.b)
colnames(df) <- str_c("ind.",1:100)       

### calculate summary data

trait.mean1 <- apply(df, 1, mean)
trait.sd1 <- apply(df, 1, sd) 

time <- 1:100000
raw.trait <- as.data.frame(cbind(time, trait.mean1,trait.sd1))

h.raw <- head(raw.trait, 1000)

#install.packages("ggplot2")
library(ggplot2)
### install.packages("reshape2")
library(reshape2)
### library(plyr)


####################### RIBBON PLOTS #################################

### ribbon plot with gg plot
### data goes here, change x,y to your liking

ggplot(data=h.raw, aes(x=time, y=trait.mean1)) + geom_ribbon(aes(ymin=trait.mean1-2*trait.sd1, ymax=trait.mean1+2*trait.sd1), color="#9933FF", fill="#9933FF") + geom_line() + scale_x_continuous("time (in generations)") + scale_y_continuous("trait value")


##################### GENERATE DATA SET 2 ############################

### secondary data set for comparison t

time_steps = 100000
X2 = replicate(100, cumsum(c(0, rnorm(time_steps - 1))))
d2 <- as.data.frame(X2)
colnames(d2) <- str_c("ind.",1:100)

trait.mean2 <- apply(d2, 1, mean)
trait.sd2 <- apply(d2, 1, sd)

#################### CALCULATE DIFF FROM MEAN ########################

### full set of differences (for future multiple t-tests)

diff2 <- d2-trait.mean2
diff1 <- df-trait.mean1

### perform ttest on end data

#results <- t.test(diff1[100000,], diff2[100000,])
#results

################## GRAPH MEAN CHANGE IN DIFF V. TIME #################

diff.mean1 <- apply(diff1, 1, mean)
diff.mean2 <- apply(diff2, 1, mean)
diff.mean.change <- diff.mean1-diff.mean2
dmc.g <- cbind(time, diff.mean.change)

dmc.df <- as.data.frame(dmc.g)

ggplot(data=dmc.df, aes(x=time, y=diff.mean.change)) + geom_line() + geom_hline(yintercept=0, color="orange", linetype=2) + scale_x_continuous("time (in generations)") + scale_y_continuous("difference of mean difference")

################## GRAPH BOTH SDS V. TIME ############################

sds <- cbind(time, trait.sd1, trait.sd2)
sd.compare <- melt(as.data.frame(sds), id.vars="time")

ggplot(data=sd.compare, aes(x=time, y=value, color=variable)) + geom_line()

################# GRAPH CORRECTED CHANGE IN SD V. TIME ###############

sd.diff <- trait.sd2 - trait.sd1
trait.comb <- cbind(trait.sd1, trait.sd2)
mean.traits <- apply(trait.comb, 1, mean)
sd.corrected <- sd.diff/mean.traits
sd.corrected[1] <- 0
sd.diff.graph <- cbind(time,sd.corrected)

sd.diff.df <- as.data.frame(sd.diff.graph)

ggplot(data=sd.diff.df, aes(x=time, y=sd.corrected)) + geom_line() + geom_hline(yintercept=0, color="orange", linetype=2) + scale_x_continuous("time (in generations)") + scale_y_continuous("percent change in sd")


