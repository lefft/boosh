### playground for trying different parameters on simulate_mh()

# load dependencies
library("dplyr"); library("ggplot2")

# load functions for simulating mh problem
source("monty_functions.r")

# simulate the problem with default parameters and summarize
summarize_sims(simulate_mh())

# simulate the problem for one strategy one time
summarize_sims(simulate_mh(num_runs=1, strategies="stay"))

# simulate the problem with specific num runs (half of each strategy)
summarize_sims(simulate_mh(num_runs=10))


