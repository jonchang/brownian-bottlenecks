#!/usr/bin/env Rscript

library(multicore)
library(reshape2)
source("sim_library.R")

num.runs <- 20
folder <- "runs/"
options(cores=4)

params <- list(individuals           = 1000,
			   loci                  = 1000,
			   mutation.rate         = 4 * 10^-6,
			   generations           = 10^6 + 10^5,
			   bottleneck.proportion = 0.01,
			   bottleneck.times      = c(0.1, 0.15, 0.2, 0.25, 0.3)
)
bottleneck.times.single <- 0.1

save.run <- function(parms, filename, options) {
	results <- run.sim(parms, sample.every=parms$generations/200,
					   options=options)
	write.table(results, file=filename, col.names=FALSE)
	lastrow <- results[nrow(results), ]
	return(lastrow)
}

brownian.only <- function(parms, index) {
	filename <- paste(folder, "brownian_", index,
					  ".txt", sep="")
	save.run(parms, filename, c("mutation"))
}

multi.bottleneck <- function(parms, index) {
	filename <- paste(folder, "multi_bottleneck_", index,
					  ".txt", sep="")
	save.run(parms, filename, c("mutation", "bottleneck"))
}

single.bottleneck <- function(parms, index) {
	filename <- paste(folder, "single_bottleneck_", index,
					  ".txt", sep="")
	save.run(parms, filename, c("mutation", "bottleneck"))
}


dir.create(folder, showWarnings=FALSE, recursive=TRUE)

brown <- parallel(mclapply(seq_len(num.runs), brownian.only, parms=params))
mbottle <- parallel(mclapply(seq_len(num.runs), multi.bottleneck, parms=params))
	
# different bottleneck time for the single run
params[["bottleneck.times"]] <- bottleneck.times.single
sbottle <- parallel(mclapply(seq_len(num.runs), single.bottleneck, parms=params))
alljobs <- collect(list(brown, mbottle, sbottle))

res <- alljobs[as.character(brown$pid)]
df <- data.frame(res)
colnames(df) <- seq_len(num.runs)
write.table(df, file=paste(folder, "brownian_summary.txt", sep=""))

res <- alljobs[as.character(mbottle$pid)]
df <- data.frame(res)
colnames(df) <- seq_len(num.runs)
write.table(df, file=paste(folder, "multi_bottleneck_summary.txt", sep=""))

res <- alljobs[as.character(sbottle$pid)]
df <- data.frame(res)
colnames(df) <- seq_len(num.runs)
write.table(df, file=paste(folder, "single_bottleneck_summary.txt", sep=""))

