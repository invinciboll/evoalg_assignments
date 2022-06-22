# Josef Mayer, Sebastian Boll
setwd("C:\\Users\\sboll\\studium\\EvoAlg\\Assignments\\assignment_07")

load_rdata <- function(filename) {
    # Object needs to be named 'params'
    load(filename)
    return(params)
}

init <- function(population_size) {
    population <- matrix(, nrow = population_size, ncol = 3)
    colnames(population) <- c("x", "y", "fitness")
    for (i in seq_len(nrow(population))) {
        population[i, ] <- c(sample(-5:5, 2), NA)
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
    offspring <- matrix(, nrow = nrow(population), ncol = 3)
    colnames(offspring) <- c("x", "y", "fitness")
    for (i in seq_len(nrow(population))) {
        # Parents get selected without fitness bias
        # Get 2 random chosen parents (its possible to chose the same individual for both parents)
        parent_indexes <- floor(runif(2, 1, nrow(population) + 1))
        # Intermediary recombine them
        offspring[i, 1] <- mean(c(population[parent_indexes[1], 1], population[parent_indexes[2], 1]), trim = 0)
        offspring[i, 2] <- mean(c(population[parent_indexes[1], 2], population[parent_indexes[2], 2]), trim = 0)
        offspring[i, 3] <- NA
    }
    return(offspring)
}

# Select survivors by 5+5 strategy
select_survivors <- function(population, offspring) {
    print(population)
    print(offspring)
    all <- rbind(population, offspring)
    all <- all[order(all[, 3]), ]
    print(all)
    print(all[(nrow(all) / 2 + 1):nrow(all), ])
}

ES <- function(population_size) {
    params <- load_rdata("params.RData")
    # INITIALIZE
    population <- init(population_size)

    # EVALUATE
    for (i in seq_len(nrow(population))) {
        population[i, 3] <- fitness(population[i, 1], population[i, 2], params)
    }

    # REPEAT
    terminate <- FALSE
    while (!terminate) {
        # RECOMBINE
        offspring <- recombine(population)

        # MUTATE

        # EVALUATE
        for (i in seq_len(nrow(offspring))) {
            offspring[i, 3] <- fitness(offspring[i, 1], offspring[i, 2], params)
        }

        # SELECT
        population <- select_survivors(population, offspring)
        terminate <- TRUE
    }
}

ES(population_size = 5)