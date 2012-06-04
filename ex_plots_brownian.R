#!/usr/bin/env Rscript

library(ggplot2)
library(reshape2)

time.steps <- 100
stuff <- replicate(50, cumsum(c(0, rnorm(time.steps - 1))))
df <- as.data.frame(stuff)
df$time <- 1:time.steps
melted <- melt(df, id.vars=c("time"))
y.limits <- c(min(melted$value), max(melted$value))

stepsize <- 20
for (i in seq(stepsize, time.steps, stepsize)) {
	p <- ggplot(subset(melted, time < i), aes(time, value, group=variable)) + geom_line() + xlim(c(1, time.steps)) + ylim(y.limits)
	ggsave(paste("ex_brownian_", i, ".pdf", sep=""), p)
	q <- ggplot(subset(melted, time == i), aes(value)) + geom_histogram() + xlim(y.limits)
	ggsave(paste("ex_brownian_hist_", i, ".pdf", sep=""), q)
}