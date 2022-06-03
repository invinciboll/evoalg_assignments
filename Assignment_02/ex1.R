### Josef Mayer & Sebastian Boll

# Task a
g <- function(x) {
    return(x)
}

h <- function(x) {
    return(sin(x))
}

i <- function(x) {
    return(x * sin(x))
}

j <- function(x) {
    return(2 + cos(x) + sin(x))
}

secant <- function(xk, deltx, func) {
    res <- (func(xk + deltx) - func(xk - deltx)) / (2 * deltx)
    return(res)
}

gradient <- function(func, steps, deltx, x0, n) {
    results <- matrix(, nrow = 100, ncol = 3)
    xk <- c(x0, x0, x0)
    for (k in 1:(steps)) {
        xk <- (xk + n * secant(xk, deltx, func))
        results[k, ] <- c(xk)
    }
    return(results)
}

plot_data <- function(data, steps, name, ylim, legendx, legendy) {
    plot(c(1:steps), data[,1], main=name, xlab="x", ylab="y", type="o", col="blue", pch=".", lty=1, ylim=ylim )
    points(c(1:steps), data[,2], col="red", pch=".")
    lines(c(1:steps), data[,2], col="red",lty=2)
    points(c(1:steps), data[,3], col="dark red",pch=".")
    lines(c(1:steps), data[,3], col="dark red", lty=3)
    legend(legendx,legendy,legend=c("0.01","0.1","0.25"), col=c("blue","red","dark red"),
                                   lty=c(1,2,3), ncol=1)
}

steps <- 100
deltx <- 1
x0 <- 0
n <- c(0.01, 0.1, 0.25)

data_g <- gradient(g, steps, deltx, x0, n)
data_h <- gradient(h, steps, deltx, x0, n)
data_i <- gradient(i, steps, deltx, x0 = 1, n)
data_j <- gradient(j, steps, deltx, x0, n)

pdf("ex1a.pdf") 
par(mfrow = c(2, 2))
plot_data(data_g, steps, "g", c(0,25), 0, 25)
plot_data(data_h, steps, "h", c(0,1.7), 60, 1.4)
plot_data(data_i, steps, "i", c(1,2.2), 60, 1.45)
plot_data(data_j, steps, "j", c(0,0.9), 60, 0.35)
dev.off()

# Task b
evalute_population <- function(func, parents, offspring) {
    population_with_fitness <- matrix(, nrow = 20, ncol = 2)
    population_with_fitness[, 1] <- c(parents, offspring)
    population_with_fitness[, 2] <- func(c(parents, offspring))
    population_with_fitness <- population_with_fitness[order(population_with_fitness[,2],decreasing=TRUE),]
    return(population_with_fitness[0:10, 1])
}

ea <- function(seed, delta, generations, func, gaussian=FALSE) {
    # INITIALISE population with random candidate solutions
    set.seed(seed)
    parents <- runif(10, 0, 10)
    for (i in 1:generations) {
        # CLONE
        offspring <- parents
        # MUTATE the resulting offspring
        if (gaussian) {
            offspring <- offspring + rnorm(10)
        } else {
            offspring <- offspring + sample(c(-delta, delta), replace = TRUE, size = 10)
        }
        # EVALUATE the population
        parents <- evalute_population(func, parents, offspring)
    }
    print(parents)
}

run_ea <- function(seed, delta, generations, func) {
    ea(seed, delta[1], generations, func)
    ea(seed, delta[2], generations, func)
    ea(seed, delta[3], generations, func)
}


delta <- c(0.01, 0.1, 0.25)
generations <- 100
seed <- 4711

print("--- fixed delta")
print("g:")
run_ea(seed, delta, generations, g)
print("h:")
run_ea(seed, delta, generations, h)
print("i:")
run_ea(seed, delta, generations, i)
print("j:")
run_ea(seed, delta, generations, j)

print("--- gaussian delta")
print("g:")
ea(seed, delta, generations, g, gaussian=TRUE)
print("h:")
ea(seed, delta, generations, h, gaussian=TRUE)
print("i:")
ea(seed, delta, generations, i, gaussian=TRUE)
print("j:")
ea(seed, delta, generations, j, gaussian=TRUE)
