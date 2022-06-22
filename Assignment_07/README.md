# Information related to assignment 7
## ES Algorithm used:
```
INITIALIZE
EVALUATE
REPEAT
    No fitness bias
    RECOMBINE
    MUTATE
    EVALUATE
    SELECT
TERMIANTION?
```
|ES||
|---|---|
|Representation | Real-valued vectors|
|Speciality |Self-adaption of mutation step size |
|Parent selection | None or uniform Fitness-Proportional|
|Recombination | Discrete or intermediary|
|Mutation | Gaussian perturbation | 
|Survivor selection | $(\mu, \lambda)$ or $(\mu + \lambda)$|
|

notizen:
- jedes individum hat sein eigenes sigma -> uncorrleated mutation 
- flaches gelände -> großes sigma, feingranular -> kleines sigma
- N(i) zufallszahlen

