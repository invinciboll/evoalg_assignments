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
    niches <- matrix(, nrow = nrow(evaluated_pop) / niche_size, ncol = niche_size * 2)
    for (i in seq_len(nrow(niches))) {
        niches[i, ] <- c(evaluated_pop[i, ], evaluated_pop[i + 1, ])
    }
    # return(niches)
    return(evaluated_pop)
}

plot_func <- function(pop, fitness_func) {
    plot(pop[, 1], pop[, 2], ylim = range(-1, 1))
    lines(c(0:30), fitness_func(c(0:30)), col = "red")
}

EA <- function(pop_size = 10, niche_size = 2) {
    pop <- init(pop_size)
    print(pop)
    evaluated_pop <- fitness_sharing(pop, niche_size, f)
    print(evaluated_pop)
    plot_func(evaluated_pop, f)
}

EA()