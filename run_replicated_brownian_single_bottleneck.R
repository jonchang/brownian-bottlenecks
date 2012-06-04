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
			   bottleneck.times      = 0.2
)

save.run <- function(parms, filename, options) {
	results <- run.sim(parms, sample.every=parms$generations/20,
					   options=options)
	write.table(results, file=filename, col.names=FALSE)
	lastrow <- results[nrow(results), ]
	return(lastrow)
}

brownian.bottleneck <- function(parms, index) {
	filename <- paste("single_bottleneck/bottleneck_", index,
					  ".txt", sep="")
	save.run(parms, filename, c("mutation", "bottleneck"))
}

dir.create("single_bottleneck", showWarnings=FALSE, recursive=TRUE)

bottle <- parallel(mclapply(seq_len(num.runs), brownian.bottleneck, parms=params))
alljobs <- collect(bottle)

res <- alljobs[as.character(bottle$pid)]
df <- data.frame(res)
colnames(df) <- seq_len(num.runs)
write.table(df, file="single_bottleneck/bottleneck_summary.txt")
