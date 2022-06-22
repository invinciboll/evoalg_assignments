# # Josef Mayer, Sebastian Boll
# setwd('C:\\Users\\sboll\\studium\\EvoAlg\\Assignments\\assignment_06')

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # #  The following section contains the GA functions:
# # INITIALIZE
init <- function(population_size) {
  population <- list()
  for (i in seq_len(population_size)) {
    individual <- matrix(sample(1:100, 100), nrow = 10, ncol = 10)
    population[[i]] <- individual
  }
  return(population)
}

# # EVALUATE
evaluate_individual <- function(individual) {
  # We expect a quadratic matrix
  if (ncol(individual) != nrow(individual)) {
    return(-1)
  } else {
    l <- ncol(individual)
    magic_number <- (l+l^3)/2
    sums <- c()
    for (i in seq_len(ncol(individual))) {
      sums <- c(sums, sum(individual[, i], sum(individual[i, ])))
    }
    sums <- c(sums, sum(diag(individual)), sum(diag(individual[,rev(sequence(NCOL(individual)))])))
    n <- abs(magic_number - sums)
    fitness <- sum(n)
    return(fitness)
  }
}

# # SELECTION
sus_selector <- function(r, delta, fitness_matrix) {
  for (i in rev(seq_len(nrow(fitness_matrix)))) {
    if (r > fitness_matrix[i,4]){
      return(i+1)
    }
  }
  return(0)
}

sus <- function(fitness_matrix) {
    offspring <- list()
    # erstellen des fitness proportion vektors
    sum_fitness <- sum(fitness_matrix[, 2])
    proportions <- round(sum_fitness / fitness_matrix[, 2], 2)
    fitness_matrix <- cbind(fitness_matrix, proportions / 100)

    # Add column for lower limits of regions
    tmp <- 0
    upper_boarders <- c()
    for(i in seq_len(nrow(fitness_matrix))) {
      tmp <- tmp + fitness_matrix[i, 3]
      upper_boarders <- c(upper_boarders, tmp)
    }
    fitness_matrix <- cbind(fitness_matrix, upper_boarders)
    
    r <- round(runif(1), 4)

    # Debug:
    print(fitness_matrix)
    print("r:")
    print(r)
    print("selected:")
    print(sus_selector(r, 1, fitness_matrix))
    
    # TODO: den selector in einer schleife aufrufen um genug individuen ("IDs") zu bekommen
}


# # GA
GA <- function(population_size, max_generations) {
  # INITIALISE
  population <- init(population_size)
  
  # EVALUATE
  fitness_matrix <- matrix(nrow=population_size, ncol=2)
  for (i in seq_len(population_size)) {
    fitness <- evaluate_individual(population[[i]])
    fitness_matrix[i, ] <- c(i, fitness)
    if (fitness == 0) {
      # If one offspring has fitness == 0 we can stop right here, but that would be very lucky
      solved <- TRUE
    }
  }

  # REPEAT (until solving permutation is found or max generations are exceeded)
  current_generation <- 0
  solved <- FALSE
  while(!solved && (current_generation < max_generations)) {
    # SELECT
    selected_ids <- sus(fitness_matrix)
    # RECOMBINE
    offspring <- list()
    # MUTATE
    

    # EVALUATE offspring
    for (i in seq_len(population_size)) {
      fitness <- evaluate_individual(offspring[[i]])
      fitness_matrix[i, ] <- c(i, fitness)
      if (fitness == 0) {
        # If one offspring has fitness == 0 we can terminate the GA
        solved <- TRUE
      }
    }
    # SELECT survivors (or replacement): replace the whole population with the resulting offspring (and random choices to reach the population size)
    
    
    current_generation <- current_generation + 1
  }
  # Return the solving permutation
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #  Section for config, running

# Config here

set.seed(as.numeric(Sys.time()))

GA(population = 10, max_generations = 100)