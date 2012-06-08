#!/usr/bin/env Rscript

library(ggplot2)
library(reshape2)

filename <- "nondrift_runs/single_bottleneck_5.txt"
tt <- theme_text(size=20)
opt <- opts(axis.text.y=tt, axis.text.x=tt, axis.title.x=tt, axis.title.y=theme_text(size=20, angle=90))
width <- 8
height <- 5.5

df <- read.table(filename)
colnames(df) <- c("time", 1:1000)
melted <- melt(df, id.vars="time")

p <- ggplot(melted, aes(time, value, group=variable)) + geom_jitter(alpha=0.1, position=position_jitter(height=0.6)) + geom_vline(xintercept=100000, color="red") + coord_cartesian(xlim=c(0, 500000)) + opt
ggsave("single_run_bottleneck_example.pdf", plot=p, width=width, height=height)  # png for smaller file size