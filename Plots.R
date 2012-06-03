#!/usr/bin/Rscript

### Trait evolution plots

### For practice -- simulated browninan motion trait data

install.packages("ape")
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

trait.mean <- apply(df, 1, mean)
trait.sd <- apply(df, 1, sd) 

time <- 1:100000
raw.trait <- as.data.frame(cbind(time, trait.mean,trait.sd))

h.raw <- head(raw.trait, 1000)

### ribbon plot with gg plot

install.packages("ggplot2")
library(ggplot2)

### data goes here, change x,y to your liking

ggplot(data=h.raw, aes(x=time, y=trait.mean)) + geom_ribbon(aes(ymin=trait.mean-trait.sd, ymax=trait.mean+trait.sd), color="#9933FF", fill="#9933FF") + geom_line() + scale_x_continuous("time (in generations)") + scale_y_continuous("trait value")

### convert to long form

install.packages("reshape2")
library(reshape2)
library(plyr)