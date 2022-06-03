# Task 1
setwd('C:\\Users\\sboll\\studium\\EvoAlg\\Assignments')
data <- read.csv(file = "assignment_03/data1.txt", sep = " ", header = FALSE)

# Algorithm from exercise sheet
perceptron <- function(omega, data, l = 1) {
    while (l != 0) {
    l <- 0
    for (mu in 1:nrow(data)) {
        y_mu <- data[mu, 4]
        x_mu <- c(data[mu, 1], data[mu, 2], data[mu, 3])
        s <- sign(x_mu %*% omega)
        delta <- (y_mu - s)
        #cat("\ny: ", y_mu, "\tscalar: ", (x_mu %*% omega),"sign: ", s, "\tdelta: ", delta)
        if (delta != 0) {
            l <- l + 1
            omega <- omega + delta * x_mu
        }
        }
    }
    return(omega)
}

# Train omega
omega <- perceptron(c(0,0,0), data)

# Test omega on our data
result <- matrix(nrow = 200, ncol = 4)
h <- function(x) {
    return(sign(x %*% omega))
}

for (i in 1:nrow(data)) {
        x_i <- c(data[i, 1], data[i, 2], data[i, 3])
        y <- h(x_i)
        result[i, ] <- c(x_i, y)
}

# Plot orignal data, test data and omega
par(mfrow = c(2, 1))
plot(data$V1, data$V2, col = ifelse(data$V4 < 0,"red","blue"), pch = 19)
plot(result[,1], result[,2], col = ifelse(result[,4] < 0,"red","blue"), pch = 19)
print(omega[1:2])
print(2*omega[1:2])
lines(omega[1:2], col = "black", lty = 1)
lines(2*omega[1:2], col = "black", lty = 1)
### not relevant for exercise
# Generate random data to test
# testdata <- matrix(nrow = 200, ncol = 4)
# for (i in 1:nrow(testdata)) {
#     if (i < 100) {
#         x <- c(runif(2, -5, 0), -1)
#     } else {
#         x <- c(runif(2, 0, 5), -1)
#     }
#     y <- h(x)
#     testdata[i, ] <- c(x, y)
# }
# plot(testdata[,1], testdata[,2], col = ifelse(result[,4] < 0,"red","blue"), pch = 19)