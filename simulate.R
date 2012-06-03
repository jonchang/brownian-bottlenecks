#!/usr/bin/env Rscript

individuals <- 1000
loci <- 50
mutation <- 10^-7
generation <- 10^6
bottleneck.prop <- 0.01
bottleneck.time <- c(0.05, 0.2, 0.3, 0.4, 0.5)
sample.every <- 10000

outfile <- file("sims.txt", "w")

all.gens <- matrix(nrow=generation, ncol=individuals)

prev.row <- rep.int(loci / 2, individuals)

bottleneck.at <- floor(bottleneck.time * generation)

cat("time", 1:individuals, sep="\t", file=outfile)
cat("\n", file=outfile)
cat(0, prev.row, sep="\t", file=outfile)
cat("\n", file=outfile)
for (i in 1:generation) {
	if (!(i %% sample.every)) {
		cat(i, prev.row, file=outfile, sep="\t")
		cat("\n", file=outfile)
		cat(i, mean(prev.row), var(prev.row), "\n", sep="\t")
	}
	cur <- prev.row
	# drift
	#cur <- sample(cur, length(cur), replace=TRUE)
	# mutation
	cur <- rbinom(cur, cur, mutation) + rbinom(cur, loci - cur, 1 - mutation)
	# bottleneck
	if (i %in% bottleneck.at) {
		tmp <- sample(cur, floor(bottleneck.prop * length(cur)))
		cur <- sample(tmp, length(cur), replace=TRUE)
	}
	prev.row <- cur
}

close(outfile)

df <- read.delim("sims.txt", header=T)
library(reshape2)
melted <- melt(df, id.vars=c("time"))
library(ggplot2)
myplot <- qplot(time, value, group=variable, data=melted, geom="jitter", alpha=0.25) + geom_vline(xintercept=bottleneck.time*generation, color="red")
ggsave(filename="plot.pdf", plot=myplot, height=8, width=12)