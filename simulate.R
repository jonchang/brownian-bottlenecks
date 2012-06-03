#!/usr/bin/env Rscript

ghetto.ou.sim <- function (params, sample.every=params[["generations"]]/100) {
	# params: individuals, loci, mutation.rate, generations,
	# bottleneck.proportion, bottleneck.times
	
	individuals <- params[["individuals"]]
	generations <- params[["generations"]]
	mutation.rate <- params[["mutation.rate"]]
	loci <- params[["loci"]]

	bottleneck.at <- floor(params[["bottleneck.times"]] * generations)
	# need to add 1 to account for first generation
	sample.gens <- matrix(nrow=(generations / sample.every) + 1,
						  ncol=individuals)
	
	prev.row <- rep.int(loci / 2, individuals)
	
	samprow <- 1
	sample.gens[samprow, ] <- prev.row

	for (i in 1:generations) {
		if (!(i %% sample.every)) {
			samprow <- samprow + 1
			sample.gens[samprow, ] <- prev.row
			cat(i, mean(prev.row), var(prev.row), sep="\t", fill=T)
		}
		
		cur <- prev.row
		# drift
		cur <- sample(cur, length(cur), replace=TRUE)
		
		# mutation
		cur <- rbinom(cur, cur, mutation.rate) +
			   rbinom(cur, loci - cur, 1 - mutation.rate)
			   
		# bottleneck
		if (i %in% bottleneck.at) {
			tmp <- sample(cur, floor(bottleneck.prop * length(cur)))
			cur <- sample(tmp, length(cur), replace=TRUE)
		}
		
		prev.row <- cur
	}
	rownames(sample.gens) <- seq(0, generations, by=sample.every)
	return(sample.gens)
}

individuals <- 1000
loci <- 50
mutation <- 10^-7
generation <- 10^5
bottleneck.prop <- 0.01
bottleneck.time <- c(0.05, 0.2, 0.3, 0.4, 0.5)
sample.every <- 5000

params <- list(individuals=individuals, loci=loci, mutation.rate=mutation, generations=generation, bottleneck.proportion=bottleneck.prop, bottleneck.times=bottleneck.time)
res <- ghetto.ou.sim(params, sample.every=sample.every)

df <- read.delim("sims.txt", header=T)
library(reshape2)
melted <- melt(df, id.vars=c("time"))
library(ggplot2)
myplot <- qplot(time, value, group=variable, data=melted, geom="jitter", alpha=0.25) + geom_vline(xintercept=bottleneck.time*generation, color="red")
ggsave(filename="plot.pdf", plot=myplot, height=8, width=12)