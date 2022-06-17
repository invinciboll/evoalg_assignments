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

# GA
# GA <- function(population_size, generations, op_recombination, recombine_prob, op_mutation, mutate_prob, selection_pressure) {

GA <- function(population_size) {
  # INITIALISE
  population <- init(population_size)

  # EVALUATE
  fitness_matrix <- matrix(nrow=population_size, ncol=2)

  for (i in seq_len(population_size)) {
    fitness_matrix[i, ] <- c(i, evaluate_individual(population[[i]]))
  }
  print(fitness_matrix)

  # REPEAT
  # for (i in seq_len(generations)){
    # SELECT
    
    # RECOMBINE
    # MUTATE
    # EVALUATE offspring
    # SELECT survivors (or replacement): replace the whole population with the resulting offspring (and random choices to reach the population size)

  # }
  # Return something
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #  Section for config, running

# Config here

set.seed(as.numeric(Sys.time()))

GA(10)