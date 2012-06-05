# Cheat Sheet

## Points about Statistical Tests
* Kurtosis
    * Kurtosis of 3 = Normal Distribution
    * Kurtosis of 1.8 = Uniform Distribution
    * Kurtosis > 3 = peaked distribution
* Skewness
    * Skewness of 0 = Normal Distribution
    * Skewness > 0 = Positive Skew/Skewed Right
    * Skewness < 0 = Negative Skew/Skewed Left

## How we chose stuff
* Mutation rate
    * Took per gen per bp
    * Chose average gene length
    * Assumed first two sites 4 fold degenerate
    * Assumed any of those changes would change it back
        * Probably a mistake
* Time Scale
	* Some of the trees we've looked at 60 mya
	* Boxfishes 15 mya
	* We assumed 1 generation = 10 years
	* Initially went with 1 mil generations
	* Then we decide to go a bit longer to 5 mil generations
## To Consider    
* Diploidy
    * Recomb UP, LD D, Var D
    * B/c rate slower, recovery time might actually be longer.
    
## How is BM, OU, AC/DC, etc. used?
* Used to estimate how traits are evolving
* Suggests different mechanisms
* Usually a statistic that tells us how likely we are to get the data with that model helps us.
    * Then use that statistic to make another statistic called AIC
    * Helps us choose which one model is more likely
    
