#!/usr/bin/env Rscript

library(ggplot2)
library(reshape2)

time.steps <- 200
numplots <- 5
width <- 5
height <- 5

stuff <- replicate(100, cumsum(c(0, rnorm(time.steps - 1))))

df <- as.data.frame(stuff)
df$time <- 1:time.steps
melted <- melt(df, id.vars=c("time"))

y.limits <- range(melted$value)
stepsize <- time.steps / numplots

for (i in seq(stepsize, time.steps, stepsize)) {
	counter <- i / time.steps * numplots
	p <- ggplot(subset(melted, time < i), aes(time, value, group=variable)) + 
		 geom_line(alpha=0.5) + xlim(c(1, time.steps)) + ylim(y.limits) + 
		 geom_hline(aes(yintercept=mean(value)), color="red")
	q <- ggplot(subset(melted, time == i), aes(value)) +
		 geom_histogram(binwidth=diff(y.limits)/10) + xlim(y.limits) +
		 geom_vline(aes(xintercept=mean(value)), color="red") + coord_flip()
	ggsave(paste("ex_brownian_hist_", counter, ".pdf", sep=""), q, width=width, height=height)
	ggsave(paste("ex_brownian_", counter, ".pdf", sep=""), p, width=width, height=height)
}