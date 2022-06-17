# # Josef Mayer, Sebastian Boll
# setwd('C:\\Users\\sboll\\studium\\EvoAlg\\Assignments\\assignment_06')


individual <- matrix(sample(1:100, 100), nrow = 10, ncol = 10)

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

evaluate_individual(individual)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # Plotting 
# plot_full_helper <- function(data, population_size, generations, name, legendx="generations", legendy="fitness") {
#     plot(c(1:generations), data[,1], main=paste(name, ", ", "population size: ", population_size), xlab=legendx, ylab=legendy, type="o", col="blue", pch=".", lty=1, ylim=range(min(data), max(data)))
#     for(i in seq_len(ncol(data))) {
#       lines(c(1:generations), data[,i], col=i,lty=2)
#     }
# }

# plot_full <- function(data1, data2, data3, data4, filename) {
#   par(mfrow = c(2, 1))
#   # standard plots
#   plot_full_helper(data1, population_size = 10, generations = 100, filename)
#   plot_full_helper(data2, population_size = 100, generations = 100, filename)
#   # Edge3 plots
#   plot_full_helper(data3, population_size = 10, generations = 100, paste(filename, " - Edge-3 "))
#   plot_full_helper(data4, population_size = 100, generations = 100, paste(filename, " - Edge-3"))
# }

# plot_avg <- function(data1, data2, data3, data4, name) {
#   par(mfrow = c(2, 1))
#   legendx = "generations"
#   legendy = "fitness"
#   plot(c(seq_len(length(data1))), data1, main=paste(name, ", ", "population size: ", 10), xlab=legendx, ylab=legendy, type="o", col="blue", pch=".", lty=1, ylim=range(min(data1), max(data1)))
#   plot(c(seq_len(length(data2))), data2, main=paste(name, ", ", "population size: ", 100), xlab=legendx, ylab=legendy, type="o", col="blue", pch=".", lty=1, ylim=range(min(data2), max(data2)))

#   plot(c(seq_len(length(data1))), data1, main=paste(name, " - Edge-3, ", "population size: ", 10), xlab=legendx, ylab=legendy, type="o", col="blue", pch=".", lty=1, ylim=range(min(data3), max(data3)))
#   plot(c(seq_len(length(data2))), data2, main=paste(name, " - Edge-3, ", "population size: ", 100), xlab=legendx, ylab=legendy, type="o", col="blue", pch=".", lty=1, ylim=range(min(data4), max(data4)))
# }

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # #  The following section contains the GA functions:
# # INITIALIZE
# init <- function(data_matrix, population_size) {
#   nodes <- c(seq_len(nrow(data_matrix)))
#   # Generate parent population
#   population <- matrix(, nrow = population_size, ncol=length(nodes) )
#   for (i in seq_len(nrow(population))) {
#         population[i, ] <- nodes[sample(length(nodes))]
#   }
#   return(population)
# }


# # GA
# GA <- function(filename, population_size, generations, op_recombination, recombine_prob, op_mutation, mutate_prob, selection_pressure, render_graph) {
#   # Crate result matrix for plotting
#   fitness_development <- matrix(,nrow=0, ncol=population_size)

#   # INITIALISE
#   population <- init(dist1, population_size)
#   # EVALUATE
#   fitness_matrix <- evaluate(dist1, population)

#   # REPEAT
#   for (i in seq_len(generations)){
#     # SELECT
#     selected_individuals_indices <- parent_select(fitness_matrix, selection_pressure)
#     selected_individuals <- matrix(, nrow = population_size, ncol = ncol(population))
#     for (i in seq_len(length(selected_individuals_indices))) {
#       selected_individuals[i, ] <- population[selected_individuals_indices[i], ]
#     }
#     # RECOMBINE
#     offspring <- op_recombination(selected_individuals, recombine_prob)
#     # MUTATE
#     mutated_offspring <- mutate(offspring, op_mutation, mutate_prob)
#     # EVALUATE offspring 
#     fitness_matrix <- evaluate(dist1, mutated_offspring)
#     # SELECT survivors (or replacement): replace the whole population with the resulting offspring (and random choices to reach the population size)
#     population <- mutated_offspring

#     # For plotting
#     fitness_development <- rbind(fitness_development, c(fitness_matrix[, 2]))
#   }
#   return(fitness_development)
# }


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # #  Section for config, running
# main <- function(generations, op_recombination, recombine_prob, op_mutation, mutate_prob, selection_pressure) {

# }

# # Config here
# generations <- 100
# op_recombination <- op_basic_recomb      #op_basic_recomb or op_edge3 (edge3 gets now plotted anyways)
# recombine_prob <- 0.75
# op_mutation <- op_swap_mutation   #op_insert_mutation or op_swap_mutation
# mutate_prob <- 0.025
# selection_pressure <- 1.2
# render_graph <- FALSE

# set.seed(as.numeric(Sys.time()))
# main(generations, op_recombination, recombine_prob, op_mutation, mutate_prob, selection_pressure, render_graph)
