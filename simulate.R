#!/usr/bin/env Rscript

individuals <- 1000
loci <- 100
mutation <- 10^-5
generation <- 10^5
drift.prop <- 0.9
bottleneck.prop <- 0.1

all.gens <- matrix(nrow=generation, ncol=individuals)

all.gens[1, ] <- loci / 2
for (i in 2:generation) {
	cur <- all.gens[i - 1, ]
	# mutation
	cur <- rbinom(cur, cur, mutation) + rbinom(cur, loci - cur, 1 - mutation)
	# drift
	cur <- sample(cur, length(cur), replace=TRUE)
	all.gens[i, ] <- cur
}

hist(all.gens[generation, ])

