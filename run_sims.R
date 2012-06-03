#!/usr/bin/env Rscript

source("sim_library.R")

individuals <- 1000
loci <- 1000
mutation <- 10^-7
generation <- 10^6
bottleneck.prop <- 0.01
bottleneck.time <- c(0.2)
sample.every <- 10000

params <- list(individuals=individuals, loci=loci, mutation.rate=mutation, generations=generation, bottleneck.proportion=bottleneck.prop, bottleneck.times=bottleneck.time)
res <- ghetto.ou.sim(params, sample.every=sample.every, options=c("mutation"))

df <- as.data.frame(res)
df$time <- as.numeric(rownames(res))
library(reshape2)
melted <- melt(df, id.vars=c("time"))
library(ggplot2)
myplot <- qplot(time, value, group=variable, data=melted, geom="jitter", alpha=0.25)  + geom_vline(xintercept=bottleneck.time*generation, color="red")
myplot
ggsave(filename="plot.pdf", plot=myplot, height=8, width=12)

