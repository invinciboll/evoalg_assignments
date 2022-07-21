f <- function(x) {
    return(sin(x))
}

init <- function(pop_size) {
    pop <- sample(0:30, 10)
    return(pop)
}

fitness_sharing <- function(pop, niche_size, fitness_func) {
    evaluated_pop <- matrix(, nrow = length(pop), ncol = 2)
    evaluated_pop[, 1] <- pop

    for (i in seq_len(nrow(evaluated_pop))) {
        evaluated_pop[i, 2] <- fitness_func(evaluated_pop[i, 1])
    }
    evaluated_pop <- evaluated_pop[order(evaluated_pop[, 2], decreasing = TRUE), ]

    # Put individuals into niches
    return(evaluated_pop)
}

EA <- function(pop_size = 10, niche_size = 2) {
    pop <- init(pop_size)
    print(pop)
    evaluated_pop <- fitness_sharing(pop, niche_size, f)
    print(evaluated_pop)
}

EA()