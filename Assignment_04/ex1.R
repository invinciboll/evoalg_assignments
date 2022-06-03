# Josef Mayer, Sebastian Boll
setwd('C:\\Users\\sboll\\studium\\EvoAlg\\Assignments\\assignment_04')


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # Helper functions:
# Input data, and (optional) render the graph
read_data <- function(filename, render_graph) {
  data <- read.csv(file = filename, sep = " ", header = FALSE)

  
  if (plot) {
    # Render graph from distance matrix
    x <- as.dist(t(data))
    library(qgraph)
    jpeg(paste(filename, ".jpg", sep="", collapse=NULL), width=1000, height=1000, unit='px')
    qgraph(x, layout='spring', vsize=3, esize=2)
    dev.off()
  }

  # Return data as symmetric matrix (for easier querying later)
  return(Matrix::forceSymmetric(t(as.matrix(data)),uplo="L"))
}

# Plotting of the fitness developement
plot_data <- function(data, generations, name, legendx, legendy) {
    plot(c(1:generations), data[,1], main=name, xlab=legendx, ylab=legendy, type="o", col="blue", pch=".", lty=1, ylim=range(20, 80))
    for(i in seq_len(ncol(data))) {
      lines(c(1:generations), data[,i], col=i,lty=2)
    }
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# #  The following section contains the GA functions:
# INITIALIZE
init <- function(data_matrix, population_size) {
  nodes <- c(seq_len(nrow(data_matrix)))
  # Generate parent population
  population <- matrix(, nrow = population_size, ncol=length(nodes) )
  for (i in seq_len(nrow(population))) {
        population[i, ] <- nodes[sample(length(nodes))]
  }
  return(population)
}


# EVALUATE
evaluate <- function(distance_matrix, population) {
  # Lower value is better
  fitness_matrix <- matrix(, nrow = nrow(population), ncol = 2)
  for (i in seq_len(nrow(population))) {
    # Create row vector to "close the ring"
    row_vector <- c(population[i, ], population[i, 1])
    fitness <- 0
    for(j in seq_len(ncol(population))) {
      node_a <- row_vector[j]
      node_b <- row_vector[j + 1]
      fitness <- fitness + distance_matrix[node_a, node_b]
    }
    fitness_matrix[i, ] <- c(i, fitness)
  }
  return(fitness_matrix)
}


# SELECTION
goldberg_select <- function(fitness_matrix, selection_pressure) {
  # Not a real goldberg, because the fitness values are very close together, so i use a function to scale the better fits
  # Create a sample vector as "roulette wheel", add weight the smaller (better) the fitness is
  # Order by worst fitness
  fitness_matrix <- fitness_matrix[order(fitness_matrix[, 2], decreasing = TRUE),]
  sample_vector <- c()
  for (i in seq_len(nrow(fitness_matrix))) {
    sample_vector <- c(sample_vector, replicate(floor(i^selection_pressure), fitness_matrix[i,1]))
  }

  # Pick random samples from our "roulette wheel"
  selected_individuals <- sample(sample_vector, nrow(fitness_matrix))
  return(selected_individuals)
}


# RECOMBINE
recombine <- function(selected_individuals, operator, recombine_prob) {
  offspring <- matrix(, nrow = nrow(selected_individuals), ncol = ncol(selected_individuals))
  
  i <- 1
  while (i < nrow(selected_individuals)){
    # Select consecutive pair of individuals (parents)
    indv_a <- selected_individuals[i, ]
    indv_b <- selected_individuals[i + 1, ]

    # Recombine ?
    if (sample(c(FALSE, TRUE), 1, prob = c(1 - recombine_prob, recombine_prob))) {
      # Yes!
      new_indv_list <- operator(indv_a, indv_b)
      offspring[i, ] <- new_indv_list[[1]]
      offspring[i + 1, ] <- new_indv_list[[2]]
    } else {
      # No!
      offspring[i, ] <- indv_a
      offspring[i + 1, ] <- indv_b
    }

    i <- i + 2
  }
  return(offspring)
}

op_basic_recomb <- function(indv_a, indv_b) {
  # From script_03 page 44
  # Note: We expect indv a and b to have the same length
  # Choose random crossover point, but guarantee that it splits the bitspring ("-1")
  cp <- floor(runif(1, 1, (length(indv_a) - 1)))

  # Copy first part into children
  new_a <- c(indv_a[1:cp])
  new_b <- c(indv_b[1:cp])

  # Iterate over remaining indicies
  cp <- cp + 1
  end_a <- c()
  end_b <- c()
  for (i in cp:length(indv_a)) {
    candidate_a <- indv_a[i]
    # Check if candidate is already in child. Yes -> Skip, No -> Append  
    if (!(candidate_a %in% new_b)) {
      new_b <- c(new_b, candidate_a)
    } else {
      end_a <- c(candidate_a, end_a)
    }
    candidate_b <- indv_b[i]
    if (!(candidate_b %in% new_a)) {
      new_a <- c(new_a, candidate_b)
    } else {
      end_b <- c(candidate_b, end_b)
    }
  }

  # Append skipped values
  new_a <- c(new_a, end_a)
  new_b <- c(new_b, end_b)

  return(list(new_a, new_b))
}

op_edge3 <- function(indv_a, indv_b) {
  # Generate edge matrix
  edge_matrix <- matrix(, nrow = length(indv_a), ncol = 5)
  for (i in seq_len(length(indv_a))){
    row <- c()
    index_a <- match(i, indv_a)
    if (index_a == 1) {
      row <- c(i, indv_a[length(indv_a)], indv_a[index_a + 1])
    } else if (index_a == length(indv_a)) {
       row <- c(i, indv_a[index_a - 1], indv_a[1])
    } else {
      row <- c(i, indv_a[index_a - 1], indv_a[index_a + 1])
    }
    index_b <- match(i, indv_b)
    if (index_b == 1) {
      row <- c(row, indv_b[length(indv_b)], indv_b[index_b + 1])
    } else if (index_b == length(indv_b)) {
      row <- c(row, indv_b[index_b - 1], indv_b[1])
    } else {
      row <- c(row, indv_b[index_b - 1], indv_b[index_b + 1])
    }
    edge_matrix[i, ] <- row
  }
  
  # Do recomb
  # Prios: 1. common element, 
}


# MUTATE
mutate <- function(offspring, operator, mutate_prob) {
  mutated_offspring <- matrix(, nrow = nrow(offspring), ncol = ncol(offspring))
  for (i in seq_len(nrow(offspring))) {
    mutated_offspring[i, ] <- operator(offspring[i, ], mutate_prob)
  }
  return(mutated_offspring)
}

op_insert_mutation <- function(individual, mutate_prob) {
  # Script p.19 "For each offspring apply mutation (a mutation rate or probability is applied at every gene)"
  for (i in seq_len(length(individual))) {
    # Mutating this gene?
    if (sample(c(FALSE, TRUE), 1, prob = c(1 - mutate_prob, mutate_prob))) {
      # Yes!
      # Get an insert partner from further into sequence
      partner <- sample((i + 1):length(individual), 1)
      # Perform insert
      mutated_individual <- c(individual[1:i], partner)
      remaining <- individual[(i + 1):length(individual)]
      # Remove partner from remaining sequence
      remaining <- remaining[!remaining %in% partner]
      # Add remaining sequence
      mutated_individual <- c(mutated_individual, remaining)

    } else {
      # No! 
      # Do nothing ...
    }
  }
  return(individual)
}

op_swap_mutation <- function(individual, mutate_prob) {
  # Script p.19 "For each offspring apply mutation (a mutation rate or probability is applied at every gene)"
  for (i in seq_len(length(individual))) {
    # Mutating this gene?
    if (sample(c(FALSE, TRUE), 1, prob = c(1 - mutate_prob, mutate_prob))) {
      # Yes!
      # Get a swap partner
      partner <- c()
      unique <- FALSE
      while (!unique) {
        partner <- sample(seq_len(length(individual)), 1)
        if (partner != i) {
          # We found a partner that is not i itself
          unique <- TRUE
        }
      }
      # Perform swap
      individual <- replace(individual, c(i, partner), individual[c(partner, i)])
    } else {
      # No! 
      # Do nothing ...
    }
  }
  return(individual)
}


# # The GA
run <- function(filename, population_size, generations, op_recombination, recombine_prob, op_mutation, mutate_prob, selection_pressure, render_graph=FALSE) {
  # Crate result matrix for plotting
  fitness_development <- matrix(,nrow=0, ncol=population_size)

  # Read data
  dist1 <- read_data(filename, render_graph)
  # INITIALISE
  population <- init(dist1, population_size)
  # EVALUATE
  fitness_matrix <- evaluate(dist1, population)

  # REPEAT
  for (i in seq_len(generations)){
    # SELECT
    selected_individuals_indices <- goldberg_select(fitness_matrix, selection_pressure)
    selected_individuals <- matrix(, nrow = population_size, ncol = ncol(population))
    for (i in seq_len(length(selected_individuals_indices))) {
      selected_individuals[i, ] <- population[selected_individuals_indices[i], ]
    }
    # RECOMBINE
    offspring <- recombine(selected_individuals, op_recombination, recombine_prob)
    # MUTATE
    mutated_offspring <- mutate(offspring, op_mutation, mutate_prob)
    # EVALUATE offspring 
    fitness_matrix <- evaluate(dist1, mutated_offspring)
    # SELECT survivors (or replacement): replace the whole population with the resulting offspring (and random choices to reach the population size)
    population <- mutated_offspring

    # For plotting
    fitness_development <- rbind(fitness_development, c(fitness_matrix[, 2]))
  }
  
  plot_data(fitness_development, generations=generations, filename, legendx="generations", legendy="fitness")
  return(fitness_development)
}

# # Run
# Warning: selection_pressure, population_size have a big influence on computational time, as well as generations of course
#fitness_development_1 <- run("dist1.txt", population_size=10, generations=100, op_recombination=op_basic_recomb, recombine_prob=0.75, op_mutation=op_insert_mutation, mutate_prob=0.025, selection_pressure=1.2)


# These settings make quite a nice plot:
fitness_development_1 <- run("dist1.txt", population_size=10, generations=100, op_recombination=op_basic_recomb, recombine_prob=0.75, op_mutation=op_swap_mutation, mutate_prob=0.025, selection_pressure=1.2)
