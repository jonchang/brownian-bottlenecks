## Purpose

Examine the effects of population bottlenecks on trait evolution. Does the pattern of Brownian trait evolution still persist after demographic events?

## Steps

1. Start with many haploid individuals with a large number of loci.
2. Each generation, simulate the effects of genetic drift and mutation.
    * Each loci has a + or - state, which represent parts of a multilocus phenotype.
    * Start all allele frequencies at 0.5.
    * Drift is simulated as binominal sampling with replacement
3. After a certain number of generations, simulate a population bottleneck in the same manner as genetic drift, but only sample from a smaller pool of the population.
4. Continue simulation, and then examine the distribution of trait values to see if it is Brownian.

## Parameters

* 1000 individuals
* 1000 loci (each "loci" is 1200 bp)
* 10^-8 - mutation rate per bp per generation
    * assume 4 fold-degeneracy at first and second site of codon. 
    * thus 1/3 of 1200 is 400
    * 4 x 10^-6 - NON-SYNONYMOUS MUTATION RATE PER LOCI PER GENERATION
* 1e6 generations 
* Bottleneck at 25% of generations elapsed
* Drift: sample 90% of individuals
* Bottleneck: sample 10% of individuals
* Sample trait values every 1,000 generations

## Plots

* Trait value over time, with the average trait value in solid line and the 95% interval representing the variance in the trait value.

  If the Brownian assumption holds for the population bottleneck scenario, the average trait value should not change from the initial trait value, and the trait variance should change in a similar way to the null (no population bottleneck) scenario.
* Difference in variance between bottleneck and null scenarios over time. If the Brownian assumption holds, the delta variance should be close to 0. Also, see if the variance changes right after the bottleneck occurs.

* Change in significance over time (could be block to show intervals of significance)

## Tests

* T-test: difference from mean (x_i - xbar) between bottleneck and no-bottleneck scenarios