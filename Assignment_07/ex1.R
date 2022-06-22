# Josef Mayer, Sebastian Boll
setwd("C:\\Users\\sboll\\studium\\EvoAlg\\Assignments\\assignment_07")

load_rdata <- function(filename) {
    # Object needs to be named 'params'
    load(filename)
    return(params)
}

init <- function(population_size) {
    population <- matrix(, nrow = population_size, ncol = 5)
    colnames(population) <- c("x", "y", "sigma_x", "sigma_y", "fitness")
    for (i in seq_len(nrow(population))) {
        population[i, ] <- c(sample(-5:5, 2), NA, NA, NA)
    }
    return(population)
}

# Fitness function from exercise sheet
fitness <- function(x, y, params) {
    result <- sum(apply(params, 2, function(p) {
        a <- ((x - p["mn.x"])^2) / (2 * p["sd.x"])
        b <- ((y - p["mn.y"])^2) / (2 * p["sd.y"])
        p["scale"] * exp(-(a + b))
    }))
    return(result)
}

# Intermediary recombination
recombine <- function(population) {
    offspring <- matrix(, nrow = nrow(population), ncol = 5)
    colnames(offspring) <- c("x", "y", "sigma_x", "sigma_y", "fitness")
    for (i in seq_len(nrow(population))) {
        # Get 1 random chosen partner (exclude the individual itself)
        partner_index <- i
        while (partner_index == i) {
            partner_index <- floor(runif(1, 1, nrow(population) + 1))
        }
        # Intermediary recombine them
        offspring[i, 1] <- mean(c(population[i, 1], population[partner_index, 1]), trim = 0)
        offspring[i, 2] <- mean(c(population[i, 2], population[partner_index, 2]), trim = 0)
        offspring[i, 3] <- NA
        offspring[i, 4] <- NA
        offspring[i, 5] <- NA
    }
    print(offspring)
    return(offspring)
}

mutate <- function() {

}

# Select survivors by 5+5 strategy
select_survivors <- function(population, offspring) {
    stopifnot(nrow(population) == nrow(offspring))

    # Compare parent with its offspring, keep the fitter one. Children are favoured in case of a draw.
    new_population <- matrix(, nrow = nrow(population), ncol = ncol(population))
    colnames(new_population) <- c("x", "y", "sigma_x", "sigma_y", "fitness")
    print(population)
    print(offspring)
    for (i in seq_len(nrow(population))) {
        if (population[i, 5] > offspring[i, 5]) {
            new_population[i, ] <- population[i, ]
        } else if (offspring[i, 5] >= population[i, 5]) {
            new_population[i, ] <- offspring[i, ]
        } else {
            stop()
        }
    }
    return(new_population)
}

ES <- function(population_size) {
    params <- load_rdata("params.RData")

    # INITIALIZE
    population <- init(population_size)

    # EVALUATE
    for (i in seq_len(nrow(population))) {
        population[, 5] <- fitness(population[i, 1], population[i, 2], params)
    }

    # REPEAT
    terminate <- FALSE
    while (!terminate) {
        # RECOMBINE
        offspring <- recombine(population)

        # MUTATE

        # EVALUATE offspring
        for (i in seq_len(nrow(offspring))) {
            offspring[i, 5] <- fitness(offspring[i, 1], offspring[i, 2], params)
        }

        # SELECT

        population <- select_survivors(population, offspring)

        terminate <- TRUE
    }
}

ES(population_size = 5)