# Josef Mayer, Sebastian Boll
setwd("C:\\Users\\sboll\\studium\\EvoAlg\\Assignments\\assignment_07")
library("mvtnorm")

# Code Sections
# 1. Helper functions
# 2. ES functions and algorithm
# 3. Run section given data
# 4. Run section own data


# # # 1. Helper functions
load_rdata <- function(filename) {
    # Object needs to be named 'params'
    load(filename)
    return(params)
}

plot_gaussians <- function(params, population, first_run, z, current_generation) {
    x.points <- seq(-10, 10, length.out = 100)
    y.points <- x.points

    if (first_run) {
        for (k in seq_len(ncol(params))) {
            mu <- c(params[1, k], params[2, k])
            sigma <- matrix(c(params[3, k], 0, 0, params[4, k]), ncol = 2)
            for (i in 1:100) {
                for (j in 1:100) {
                    z[i, j] <- z[i, j] + params[5, k] * dmvnorm(c(x.points[i], y.points[j]), mean = mu, sigma = sigma)
                }
            }
            cat("\r\t\t\t\tPlotting gaussians:\t", floor(k / ncol(params) * 100), "%")
        }
    }


    filled.contour(x.points, y.points, z, nlevels = 10, plot.axes = {
        points(population[1, ], population[2, ], plot.title = title(paste("Generations: ", current_generation)))
    })

    return(z)
}

plot_fitness_evolution <- function(data, generations) {
    leg <- c()
    plot(c(1:generations), data[, 1], plot.title = title("Fitness evolution"), xlab = "Generation", ylab = "Fitness", type = "o", col = "blue", pch = ".", lty = 1, ylim = range(min(data), max(data)))
    for (i in seq_len(ncol(data))) {
        lines(c(1:generations), data[, i], col = i, lty = 1)
        leg <- c(leg, paste("Individual", i))
    }
    legend("bottomright", "(x,y)",
        legend = leg,
        lty = rep(1, ncol(data)), ncol = 1, box.lty = 0, col = seq(i:ncol(data))
    )
}

plot_sigma_evolution <- function(sigma_x_evolution, sigma_y_evolution, generations) {
    plot(c(1:generations), sigma_x_evolution[, 1], plot.title = title("Sigma evolution"), xlab = "Generation", ylab = "Sigma", type = "o", col = "blue", pch = ".", lty = 1, ylim = range(min(c(min(sigma_x_evolution), min(sigma_y_evolution))), max(c(max(sigma_x_evolution), max(sigma_y_evolution)))))
    for (i in seq_len(ncol(sigma_x_evolution))) {
        lines(c(1:generations), sigma_x_evolution[, i], col = i, lty = 1)
    }
    for (i in seq_len(ncol(sigma_y_evolution))) {
        lines(c(1:generations), sigma_y_evolution[, i], col = i, lty = 2)
    }
    legend("bottomright", "(x,y)",
        legend = c("sigma_x", "sigma_y"),
        lty = c(1, 2), ncol = 1, box.lty = 0,
    )
}

# # # 2. ES functions
init <- function(population_size) {
    population <- matrix(, nrow = population_size, ncol = 5)
    colnames(population) <- c("x", "y", "sig_x", "sig_y", "fitness")
    sigmas <- c(4, 4)
    for (i in seq_len(nrow(population))) {
        population[i, ] <- c(sample(-5:5, 2), sigmas, NA)
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
    colnames(offspring) <- c("x", "y", "sig_x", "sig_y", "fitness")
    for (i in seq_len(nrow(population))) {
        # Get 1 random chosen partner (exclude the individual itself)
        partner_index <- i
        while (partner_index == i) {
            partner_index <- floor(runif(1, 1, nrow(population) + 1))
        }
        partner_index <- floor(runif(1, 1, nrow(population) + 1))
        # Intermediary recombine them
        offspring[i, 1] <- mean(c(population[i, 1], population[partner_index, 1]), trim = 0)
        offspring[i, 2] <- mean(c(population[i, 2], population[partner_index, 2]), trim = 0)
        offspring[i, 3] <- mean(c(population[i, 3], population[partner_index, 3]), trim = 0)
        offspring[i, 4] <- mean(c(population[i, 4], population[partner_index, 4]), trim = 0)
        offspring[i, 5] <- NA
    }

    return(offspring)
}

# Self adapting non-correlating mutation with a sigma for each chromosome
mutate <- function(offspring) {
    learning_rate <- 1 # 2 chromosomes (x, y) and 2 sigmas
    # Mutate sigma
    tau_a <- (1 / sqrt(2 * learning_rate))
    tau_b <- (1 / (2 * sqrt(learning_rate)))

    for (j in seq_len(nrow(offspring))) {
        N <- runif(1, -1, 1)
        for (i in seq_len(2)) { # 2 sigmas
            N_i <- runif(1, -1, 1)
            offspring[j, i + 2] <- offspring[j, i + 2] * exp(tau_a * N + tau_b * N_i)
            offspring[j, i] <- offspring[j, i] + offspring[j, i + 2] * N_i
        }
    }
    return(offspring)
}

# Select survivors by 5+5 strategy
select_survivors <- function(population, offspring) {
    stopifnot(nrow(population) == nrow(offspring))

    # Compare parent with its offspring, keep the fitter one. Children are favoured in case of a draw.
    new_population <- matrix(, nrow = nrow(population), ncol = ncol(population))
    colnames(new_population) <- c("x", "y", "sig_x", "sig_y", "fitness")

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

ES <- function(population_size, generations, filename, filename_plots) {
    params <- load_rdata(filename)
    fitness_evolution <- matrix(, nrow = 0, ncol = population_size)
    sigma_x_evolution <- matrix(, nrow = 0, ncol = population_size)
    sigma_y_evolution <- matrix(, nrow = 0, ncol = population_size)
    cat("\tStarting...\n")

    # INITIALIZE
    population <- init(population_size)

    # EVALUATE
    for (i in seq_len(nrow(population))) {
        population[i, 5] <- fitness(population[i, 1], population[i, 2], params)
    }

    # REPEAT
    pdf(file = filename_plots)
    current_generation <- 1

    # Boolean to improve plotting speed
    first_run <- TRUE

    while (current_generation <= generations) {
        # RECOMBINE
        offspring <- recombine(population)

        # MUTATE
        offspring <- mutate(offspring)


        # EVALUATE offspring
        for (i in seq_len(nrow(offspring))) {
            offspring[i, 5] <- fitness(offspring[i, 1], offspring[i, 2], params)
        }

        # SELECT
        population <- select_survivors(population, offspring)

        cat("\r\tES Progress:\t", floor(current_generation / generations * 100), "%,")
        # Generate first + 4 plots per ES run
        if (((current_generation %% (generations / 4)) == 0) || first_run) {
            z <- plot_gaussians(params, population, first_run, z, current_generation)
            first_run <- FALSE
        }

        fitness_evolution <- rbind(fitness_evolution, c(population[5, ]))
        sigma_x_evolution <- rbind(sigma_x_evolution, c(population[, 3]))
        sigma_y_evolution <- rbind(sigma_y_evolution, c(population[, 4]))
        current_generation <- current_generation + 1
    }

    plot_fitness_evolution(fitness_evolution, generations)
    plot_sigma_evolution(sigma_x_evolution, sigma_y_evolution, generations)
    dev.off()
    cat("\n\tDone.")
}


# # # 3. Run with given params data
cat("ES with given data:\n")
z <- matrix(0, nrow = 100, ncol = 100) # relevant for plotting
ES(population_size = 5, generations = 1000, filename = "params.RData", filename_plots = "plots-given-data.pdf")

# # 4. Run with given own data
cat("\n\nES with own data:\n")
own_data_filename <- "own_params.RData"
if (!file.exists((own_data_filename))) {
    cat("\tOwn data file not found. Generating new gaussian data...\n")

    params <- data.frame(row.names = c("mn.x", "mn.y", "sd.x", "sd.y", "scale"), stringsAsFactors = default.stringsAsFactors())
    for (i in seq_len(10)) {
        data <- c(runif(2, min = -7, max = 7), runif(2, min = 1, max = 3), runif(1, min = 0.5, max = 2))
        params$NewCen <- data
        names(params)[names(params) == "NewCen"] <- gsub(" ", "", paste("Cen", i, collapse = NULL))
    }
    save(params, file = own_data_filename)
    cat("\tGaussian data generated.\n")
} else {
    cat("\tOwn data file found!\n")
}


z <- matrix(0, nrow = 100, ncol = 100) # relevant for plotting
ES(population_size = 5, generations = 1000, filename = own_data_filename, filename_plots = "plots-own-data.pdf")