# Josef Mayer, Sebastian Boll
## Pseudocode of the GA (from script_03 p.18, p.19):
```
INITIALIZE
EVALUATE
REPEAT
    SELECT parents (or mating selection) Fitness proportional parent selection (canonical GA)
    RECOMBINE: For each consecutive pair of parents apply crossover with probability , otherwise copy parents (some details vary in the literature)
    MUTATE: For each offspring apply mutation (a mutation rate or probability is applied at every gene)
    EVALUATE offspring
    SELECT survivors (or replacement): replace the whole population with the resulting offspring (and random choices to reach the population size)
TERMINATION
```
Note: In the second SELECT, we just replace our population with the offspring. The amount of surviving parents can be controlled by the probability of the recombination and mutation parameters.
## Parameters for the GA
|Parameter | Type | |
|------------------------|----------------|----------------------------------------------------------------------------|
|filename                |[String]        | Filename of input data |
|population_size         |[Integer]       | Size of the population |
|generations             |[Integer]       | Number of generations (GA loops) |
|op_recombination        |[Function]      | Pass the operator you want to use {op_basic_recomb | op_edge3} |
|recombine_prob          |[Float]         | Probability to apply recombination on pair of consecutive parents |
|op_mutation             |[Function]      | Pass the operator you want to use {op_insert_mutation | op_swap_mutation} |
|mutate_prob             |[Float]         | Probability to apply mutation on a gene of an individual |
|selection_pressure      |[Float] > 1     | Slection pressure for parent selection |
|render_graph            |[Boolean]       | Set TRUE if you want to render the graph from the given distance matrix |(default=FALSE)

## Observations
|||
|-|-|
|Population size | Increasing the population size leads to better results in less generations. This is logical because with a higher population we have more chances to evolve towards a better fitness in each generation. |
|Generations | Increasing the generations can yield better results until a threshold is reached, where no further improvements can be seen.|
|Recombine operator | |
|Recombine probability| |
|Mutation operator| |
|Mutation probability/rate| Its difficult to find the right amount of mutations. If we use too many mutations we lose our progress because neighborhoods get destroyed. If we use not too less mutation, we can't achieve good fitness values because the population might get too homogenous. |
|Selection pressure| Higher selection pressure kills the bad individuals early, but can result in a too homogenous population, where no further improvements can be achieved besides using heavy mutations. |