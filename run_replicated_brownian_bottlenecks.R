#!/usr/bin/env Rscript

library(multicore)
library(reshape2)
source("sim_library.R")

num.runs <- 20

params <- list(individuals           = 1000,
			   loci                  = 1000,
			   mutation.rate         = 10^-7,
			   generations           = 10^6,
			   bottleneck.proportion = 0.01,
			   bottleneck.times      = c(0.2, 0.3, 0.4, 0.5, 0.6)
)

save.run <- function(parms, filename, options) {
	results <- run.sim(parms, sample.every=parms$generations/20,
					   options=options)
	write.table(results, file=filename, col.names=FALSE)
	lastrow <- results[nrow(results), ]
	return(lastrow)
}

brownian.only <- function(parms, index) {
	filename <- paste("brownian_replicates/brownian_", index,
					  ".txt", sep="")
	save.run(parms, filename, c("mutation"))
}

brownian.bottleneck <- function(parms, index) {
	filename <- paste("brownian_replicates/bottleneck_", index,
					  ".txt", sep="")
	save.run(parms, filename, c("mutation", "bottleneck"))
}

dir.create("brownian_replicates", showWarnings=FALSE, recursive=TRUE)

brown <- parallel(mclapply(seq_len(num.runs), brownian.only, parms=params))
bottle <- parallel(mclapply(seq_len(num.runs), brownian.bottleneck, parms=params))
alljobs <- collect(list(brown, bottle))

res <- alljobs[as.character(brown$pid)]
df <- data.frame(res)
colnames(df) <- seq_len(num.runs)
write.table(df, file="brownian_replicates/brownian_summary.txt")

res <- alljobs[as.character(bottle$pid)]
df <- data.frame(res)
colnames(df) <- seq_len(num.runs)
write.table(df, file="brownian_replicates/bottleneck_summary.txt")
