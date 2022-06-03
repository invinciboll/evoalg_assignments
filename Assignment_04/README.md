# Pseudocode of the GA (from script):
INITIALIZE
EVALUATE
REPEAT
    SELECT parents (or mating selection) Fitness proportional parent selection (canonical GA)
    RECOMBINE: For each consecutive pair of parents apply crossover with probability , otherwise copy parents (some details vary in the literature)
    MUTATE: For each offspring apply mutation (a mutation rate or probability is applied at every gene)
    EVALUATE offspring
    SELECT survivors (or replacement): replace the whole population with the resulting offspring (and random choices to reach the population size)
TERMINATION?

# Parameters for the GA
filename            =   Filename of input data
population_size     =   Size of the population
generations         =   
op_recombination    =   operator  
recombine_prob      =   
op_mutation
mutate_prob
selection_pressure
plot=FALSE
