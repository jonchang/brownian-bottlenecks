#!/usr/bin/env Rscript

run.sim <- function (params, sample.every=params[["generations"]]/100,
					 options=c("drift", "mutation", "bottleneck")) {

	ok.options <- c("drift", "mutation", "bottleneck")
	options <- match.arg(options, ok.options, several.ok=TRUE)

	individuals <- params[["individuals"]]
	generations <- params[["generations"]]
	mutation.rate <- params[["mutation.rate"]]
	bottleneck.times <- params[["bottleneck.times"]]
	bottleneck.proportion <- params[["bottleneck.proportion"]]
	loci <- params[["loci"]]

	if (generations %/% sample.every != generations / sample.every) {
		stop("generations must be a multiple of sample.every")
	}

	bottleneck.at <- floor(bottleneck.times * generations)
	# need to add 1 to account for first generation
	sample.gens <- matrix(nrow=(generations / sample.every) + 1,
						  ncol=individuals)

	prev.row <- rep.int(loci / 2, individuals)

	samprow <- 1
	sample.gens[samprow, ] <- prev.row

	for (i in 1:generations) {
		cur <- prev.row

		if ("drift" %in% options) {
			cur <- sample(cur, length(cur), replace=TRUE)
		}

		# mutation
		if ("mutation" %in% options) {
			cur <- rbinom(cur, cur, mutation.rate) +
				   rbinom(cur, loci - cur, 1 - mutation.rate)
		}

		# bottleneck
		if ("bottleneck" %in% options && i %in% bottleneck.at) {
			tmp <- sample(cur, floor(bottleneck.proportion * length(cur)))
			cur <- sample(tmp, length(cur), replace=TRUE)
		}

		prev.row <- cur

		if (!(i %% sample.every)) {
			samprow <- samprow + 1
			sample.gens[samprow, ] <- prev.row
			cat(i, mean(prev.row), var(prev.row), sep="\t", fill=T)
		}
	}
	rownames(sample.gens) <- seq(0, generations, by=sample.every)
	return(sample.gens)
}
