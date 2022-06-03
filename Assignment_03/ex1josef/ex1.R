setwd("D:/Uni/Master/1. Semester/EA/blatt3")
data <- read.csv(file = "data1.txt", sep = " ", header = FALSE)

# Algorithm

# REPEAT
# L=0
# for i to n
# delta = Label - sign(<Datenpunkt,gewicht_w>)
# if(delta != 0)
# L = L+1
# Anpassung des Gewichtsvektors
# w = w + delta*Datenpunkt
# UNTIL L = 0

dot_prod <- function(vec, w) {
  return(sum(vec * w))
}

perceptron <- function() {
  pdf("ex1_perceptron-learning.pdf")
  par(mfrow = c(2, 1))
  l <- 1
  w <- c(0, 0, 0)
  # j just for checking the number of runs
  j <- 0
  while (l != 0) {
    j <- j + 1
    l <- 0
    for (i in 1:nrow(data)) {
      #Calculate delta
      y <- data[i, 4]
      x <- c(data[i, 1], data[i, 2], data[i, 3])
      signum_x <- sign(dot_prod(x, w))
      delta <- y - signum_x
      # check if w has to be adapted
      if (delta != 0) {
        l <- l + 1
        w <- w + delta * x
      }
    }

    plot(data$V1, data$V2, main = paste("Gewichtsvektor w - Durchlauf: ", j, seq = ""), xlab = "X-Values", ylab = "Y-Values", col = ifelse(data$V4 < 0, "red", "blue"), pch = 20)
    lines(w[1:2], col = "black", lty = 2)
  }
  dev.off()
  return(w)
}

w <- perceptron()

# Traningsmenge entspricht Testmenge
test <- matrix(nrow = 200, ncol = 4)

# checking for every point if the classifier w is correct or not
miss_classifiction <- 0
for (i in 1:nrow(data)) {
  x <- c(data[i, 1], data[i, 2], data[i, 3])
  signum_x <- sign(dot_prod(x, w))
  # if signum_x and the classifier in the dataset (data[i, 4]) are identical, than w is not correct and classified a point incorrect
  if (signum_x != data[i, 4]) {
    miss_classifiction <- miss_classifiction + 1
  }
  test[i,] <- c(x, signum_x)
}
# Because miss_classification is 0 the data is linearly separable
#print(miss_classifiction)

#Plot orignal data, test data and w
pdf("ex1_final-plot.pdf")
# extending w vector for the plot
par(mfrow = c(2, 1))
plot(data$V1, data$V2, main = "Original data", xlab = "X-Values", ylab = "Y-Values", col = ifelse(data$V4 < 0, "red", "blue"), pch = 19)
plot(test[, 1], test[, 2], main = "Testdata classification", xlab = "X-Values", ylab = "Y-Values", col = ifelse(test[, 4] < 0, "red", "blue"), pch = 19)
lines(w[1:2], col = "black", lty = 2)
dev.off()