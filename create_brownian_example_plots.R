#!/usr/bin/env Rscript

library(ggplot2)
library(reshape2)

time.steps <- 200
numplots <- 5
width <- 7
height <- 7
tt <- theme_text(size=20)
opt <- opts(axis.text.y=tt, axis.text.x=tt, axis.title.x=tt, axis.title.y=theme_text(size=20, angle=90))

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
		 geom_hline(aes(yintercept=mean(value)), color="red") + opt
	q <- ggplot(subset(melted, time == i), aes(value)) + opt +
		 geom_histogram(binwidth=diff(y.limits)/10) + xlim(y.limits) +
		 geom_vline(aes(xintercept=mean(value)), color="red") + coord_flip() +
		 ylim(c(0, 50))
	ggsave(paste("ex_brownian_hist_", counter, ".pdf", sep=""), q, width=width, height=height) 
	ggsave(paste("ex_brownian_", counter, ".pdf", sep=""), p, width=width, height=height) 
}