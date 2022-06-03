# Josef Mayer, Sebastian Boll
# setwd('C:\\Users\\sboll\\studium\\EvoAlg\\Assignments\\assignment_04')


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # Helper functions:
# Input data, and (optional) render the graph
read_data <- function(filename, render_graph) {
  data <- read.csv(file = filename, sep = " ", header = FALSE)
  if (render_graph) {
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

# Plotting 
plot_full_helper <- function(data, population_size, generations, name, legendx="generations", legendy="fitness") {
    plot(c(1:generations), data[,1], main=paste(name, ", ", "population size: ", population_size), xlab=legendx, ylab=legendy, type="o", col="blue", pch=".", lty=1, ylim=range(min(data), max(data)))
    for(i in seq_len(ncol(data))) {
      lines(c(1:generations), data[,i], col=i,lty=2)
    }
}

plot_full <- function(data1, data2, data3, data4, filename) {
  par(mfrow = c(2, 1))
  # standard plots
  plot_full_helper(data1, population_size = 10, generations = 100, filename)
  plot_full_helper(data2, population_size = 100, generations = 100, filename)
  # Edge3 plots
  plot_full_helper(data3, population_size = 10, generations = 100, paste(filename, " - Edge-3 "))
  plot_full_helper(data4, population_size = 100, generations = 100, paste(filename, " - Edge-3"))
}

plot_avg <- function(data1, data2, data3, data4, name) {
  par(mfrow = c(2, 1))
  legendx = "generations"
  legendy = "fitness"
  plot(c(seq_len(length(data1))), data1, main=paste(name, ", ", "population size: ", 10), xlab=legendx, ylab=legendy, type="o", col="blue", pch=".", lty=1, ylim=range(min(data1), max(data1)))
  plot(c(seq_len(length(data2))), data2, main=paste(name, ", ", "population size: ", 100), xlab=legendx, ylab=legendy, type="o", col="blue", pch=".", lty=1, ylim=range(min(data2), max(data2)))

  plot(c(seq_len(length(data1))), data1, main=paste(name, " - Edge-3, ", "population size: ", 10), xlab=legendx, ylab=legendy, type="o", col="blue", pch=".", lty=1, ylim=range(min(data3), max(data3)))
  plot(c(seq_len(length(data2))), data2, main=paste(name, " - Edge-3, ", "population size: ", 100), xlab=legendx, ylab=legendy, type="o", col="blue", pch=".", lty=1, ylim=range(min(data4), max(data4)))
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
parent_select <- function(fitness_matrix, selection_pressure) {
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
op_basic_recomb <- function(selected_individuals, recombine_prob) {
  offspring <- matrix(, nrow = nrow(selected_individuals), ncol = ncol(selected_individuals))
  
  i <- 1
  while (i < nrow(selected_individuals)){
    # Select consecutive pair of individuals (parents)
    indv_a <- selected_individuals[i, ]
    indv_b <- selected_individuals[i + 1, ]

    # Recombine ?
    if (sample(c(FALSE, TRUE), 1, prob = c(1 - recombine_prob, recombine_prob))) {
      # Yes!
      new_indv_list <- op_basic_calc_individuals(indv_a, indv_b)
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

op_basic_calc_individuals <- function(indv_a, indv_b) {
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

# RECOMBINE
op_edge3 <- function(selected_individuals, recombine_prob) {
  offspring <- matrix(, nrow = nrow(selected_individuals), ncol = ncol(selected_individuals))
  
  i <- 1
  while (i <= nrow(selected_individuals)){
    # Select pairs of individuals
    if(i == nrow(selected_individuals)) {
      indv_a <- selected_individuals[nrow(selected_individuals), ]
      indv_b <- selected_individuals[1, ]
    } else {
      indv_a <- selected_individuals[i, ]
      indv_b <- selected_individuals[i + 1, ]
    }

    # Recombine ?
    if (sample(c(FALSE, TRUE), 1, prob = c(1 - recombine_prob, recombine_prob))) {
      # Yes!
      offspring[i, ] <- op_edge3_create_offspring(indv_a, indv_b)
      # offspring[i, ] <- new_indv_list[[1]]
      # offspring[i + 1, ] <- new_indv_list[[2]]
    } else {
      # No!
      offspring[i, ] <- indv_a
      # offspring[i + 1, ] <- indv_b
    }

    i <- i + 1
  }
  return(offspring)
}

op_edge3_create_offspring <- function(indiv_a, indiv_b) {
  # Generate edge matrix
  edge_matrix <- matrix(, nrow = length(indiv_a), ncol = 5)
  for (i in seq_len(length(indiv_a))){
    neighbours <- c()

    index_a <- match(i, indiv_a)
    if (index_a == 1) {
      neighbours <- c(i, indiv_a[length(indiv_a)], indiv_a[index_a + 1])
    } else if (index_a == length(indiv_a)) {
      neighbours <- c(i, indiv_a[index_a - 1], indiv_a[1])
    } else {
      neighbours <- c(i, indiv_a[index_a - 1], indiv_a[index_a + 1])
    }
    index_b <- match(i, indiv_b)
    if (index_b == 1) {
      neighbours <- c(neighbours, indiv_b[length(indiv_b)], indiv_b[index_b + 1])
    } else if (index_b == length(indiv_b)) {
      neighbours <- c(neighbours, indiv_b[index_b - 1], indiv_b[1])
    } else {
      neighbours <- c(neighbours, indiv_b[index_b - 1], indiv_b[index_b + 1])
    }
    edge_matrix[i, ] <- neighbours
  }
  
  # Do recomb
  return(op_edge3_create_individual(edge_matrix))
}

op_edge3_create_individual <- function(edge_matrix) {
  child <- c()
  # for 4. tracking which elements are not used yet
  not_used <- edge_matrix[ ,1]
  # 1. Pick inital element at random
  child <- append(child, sample(edge_matrix[ ,1], 1))
  # Repeat
  for(i in seq_len(length(edge_matrix[ ,1]) - 1)) {
    # 1. current element is entry
    current <- child[i]
    # 4. If reaching an empty list 
    #   - new element is choosen at random
    if(sum(edge_matrix[current, ]) == 0) {
      current <- sample(not_used, 1)
      not_used <- not_used[! not_used == current]
    }
    # 2. remove entry from the distance matrix
    for(j in 2:5) {
      for(k in seq_len(nrow(edge_matrix))) {
        # cat(edge_matrix[k,j], "==", current, "\n")
        if(edge_matrix[k,j] == current) {
          # setting the value to Inf causes that the value can not be used anymore
          edge_matrix[k,j] <- Inf
        }
      }
    }
    # 3. examine list for current element
    found_next_element <- FALSE
    neighbours <- edge_matrix[current, ]
    # print(neighbours)
    #   - common edge (+), pick that to be the next element
    for(j in 2:length(neighbours)) {
      for(k in j:length(neighbours)) {
        # check if duplocate was found and if the duplicated is not a deleted element, that is still in the matrix
        if(j == k && !found_next_element && j != Inf) {
          # found a common edge
          child <- append(child, neighbours[j])
          found_next_element <- TRUE
          not_used <- not_used[! not_used == j]
        }
      }
    }
    #   - Pick the entry in the list which has the shortest list
    if(!found_next_element) {
      neighbours_length <- c()
      for(j in 2:length(neighbours)) {
        neighbours_length <- append(neighbours_length, length(edge_matrix[j, ]))
      }
      mins <- which(neighbours_length == min(neighbours_length))
      # only one minimum found
      if(length(mins) == 1) {
        child <- append(child, which.min(neighbours_length))
        not_used <- not_used[! not_used %in% c(which.min(neighbours_length))]
      }
      #   - Ties are split at random
      # multiple minima found 
      else {
        index <- sample(mins,1)
        child <- append(child, edge_matrix[1, index])
        not_used <- not_used[! not_used %in% c(edge_matrix[1, index])]
      }
    }
    found_next_element <- FALSE
  }
  return(child)
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


# GA
GA <- function(filename, population_size, generations, op_recombination, recombine_prob, op_mutation, mutate_prob, selection_pressure, render_graph) {
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
    selected_individuals_indices <- parent_select(fitness_matrix, selection_pressure)
    selected_individuals <- matrix(, nrow = population_size, ncol = ncol(population))
    for (i in seq_len(length(selected_individuals_indices))) {
      selected_individuals[i, ] <- population[selected_individuals_indices[i], ]
    }
    # RECOMBINE
    offspring <- op_recombination(selected_individuals, recombine_prob)
    # MUTATE
    mutated_offspring <- mutate(offspring, op_mutation, mutate_prob)
    # EVALUATE offspring 
    fitness_matrix <- evaluate(dist1, mutated_offspring)
    # SELECT survivors (or replacement): replace the whole population with the resulting offspring (and random choices to reach the population size)
    population <- mutated_offspring

    # For plotting
    fitness_development <- rbind(fitness_development, c(fitness_matrix[, 2]))
  }
  return(fitness_development)
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #  Section for config, running
main <- function(filename1, filename2, filename3, generations, op_recombination, recombine_prob, op_mutation, mutate_prob, selection_pressure, render_graph=FALSE) {
  print(paste("Working on ", filename1))
  fitness_development_dist1_10 <- GA(filename1, population_size=10, generations=generations, op_recombination=op_recombination, recombine_prob=recombine_prob, op_mutation=op_mutation, mutate_prob=mutate_prob, selection_pressure=selection_pressure, render_graph=render_graph)
  fitness_development_dist1_100 <- GA(filename1, population_size=100, generations=generations, op_recombination=op_recombination, recombine_prob=recombine_prob, op_mutation=op_mutation, mutate_prob=mutate_prob, selection_pressure=selection_pressure, render_graph = render_graph)
  edge3_development_dist1_10 <- GA(filename1, population_size=10, generations=generations, op_recombination=op_edge3, recombine_prob=recombine_prob, op_mutation=op_mutation, mutate_prob=mutate_prob, selection_pressure=selection_pressure, render_graph=render_graph)
  # edge3
  edge3_development_dist1_100 <- GA(filename1, population_size=100, generations=generations, op_recombination=op_edge3, recombine_prob=recombine_prob, op_mutation=op_mutation, mutate_prob=mutate_prob, selection_pressure=selection_pressure, render_graph = render_graph)
  print(paste("Working on ", filename2))
  fitness_development_dist2_10 <- GA(filename2, population_size=10, generations=generations, op_recombination=op_recombination, recombine_prob=recombine_prob, op_mutation=op_mutation, mutate_prob=mutate_prob, selection_pressure=selection_pressure, render_graph = render_graph)
  fitness_development_dist2_100 <- GA(filename2, population_size=100, generations=generations, op_recombination=op_recombination, recombine_prob=recombine_prob, op_mutation=op_mutation, mutate_prob=mutate_prob, selection_pressure=selection_pressure, render_graph = render_graph)
  # edge3
  edge3_development_dist2_10 <- GA(filename2, population_size=10, generations=generations, op_recombination=op_edge3, recombine_prob=recombine_prob, op_mutation=op_mutation, mutate_prob=mutate_prob, selection_pressure=selection_pressure, render_graph = render_graph)
  edge3_development_dist2_100 <- GA(filename2, population_size=100, generations=generations, op_recombination=op_edge3, recombine_prob=recombine_prob, op_mutation=op_mutation, mutate_prob=mutate_prob, selection_pressure=selection_pressure, render_graph = render_graph)
  print(paste("Working on ", filename3))
  fitness_development_dist3_10 <- GA(filename3, population_size=10, generations=generations, op_recombination=op_recombination, recombine_prob=recombine_prob, op_mutation=op_mutation, mutate_prob=mutate_prob, selection_pressure=selection_pressure, render_graph = render_graph)
  fitness_development_dist3_100 <- GA(filename3, population_size=100, generations=generations, op_recombination=op_recombination, recombine_prob=recombine_prob, op_mutation=op_mutation, mutate_prob=mutate_prob, selection_pressure=selection_pressure, render_graph = render_graph)
  edge3_development_dist3_10 <- GA(filename3, population_size=10, generations=generations, op_recombination=op_edge3, recombine_prob=recombine_prob, op_mutation=op_mutation, mutate_prob=mutate_prob, selection_pressure=selection_pressure, render_graph = render_graph)
  edge3_development_dist3_100 <- GA(filename3, population_size=100, generations=generations, op_recombination=op_edge3, recombine_prob=recombine_prob, op_mutation=op_mutation, mutate_prob=mutate_prob, selection_pressure=selection_pressure, render_graph = render_graph)

  print("Plotting full plots ...")
  pdf(file = "plots-full.pdf")
  plot_full(fitness_development_dist1_10, fitness_development_dist1_100, edge3_development_dist1_10, edge3_development_dist1_100, filename1)
  plot_full(fitness_development_dist2_10, fitness_development_dist2_100, edge3_development_dist2_10, edge3_development_dist2_100, filename2)
  plot_full(fitness_development_dist3_10, fitness_development_dist3_100, edge3_development_dist3_10, edge3_development_dist3_100, filename3)
  dev.off()

  print("Plotting average fitness plots ...")
  pdf(file = "plots-avg.pdf")
  plot_avg(rowMeans(fitness_development_dist1_10), rowMeans(fitness_development_dist1_100), rowMeans(edge3_development_dist1_10), rowMeans(edge3_development_dist1_100), filename1)
  plot_avg(rowMeans(fitness_development_dist2_10), rowMeans(fitness_development_dist2_100), rowMeans(edge3_development_dist2_10), rowMeans(edge3_development_dist2_100), filename2)
  plot_avg(rowMeans(fitness_development_dist3_10), rowMeans(fitness_development_dist3_100), rowMeans(edge3_development_dist3_10), rowMeans(edge3_development_dist3_100), filename3)
  dev.off()
}

# Config here
filename1 <- "dist1.txt"
filename2 <- "dist2.txt"
filename3 <- "dist3.txt"
generations <- 100
op_recombination <- op_basic_recomb
recombine_prob <- 0.75
op_mutation <- op_swap_mutation
mutate_prob <- 0.025
selection_pressure <- 1.2
render_graph <- FALSE

set.seed(as.numeric(Sys.time()))
main(filename1, filename2, filename3, generations, op_recombination, recombine_prob, op_mutation, mutate_prob, selection_pressure, render_graph)
