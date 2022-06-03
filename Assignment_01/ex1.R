# Gruppe 17
# Sebastian Boll, Josef Mayer

sum1 <- function(m) {
  sum <- 0
  for(i in 1:dim(m)[1]) {
    for(j in 1:dim(m)[2]) {
      sum <- sum + m[i,j]
    }
  }
  return <- sum
}

# laden der C-Funktion
dyn.load("Assignments/Assignment_01/sum2.so")

sum2 <- function(mat) {
  # Übergeben der Anzahl der Zeilen, Spalten und des Ergebnispointers
  return <- .C("sum2", as.double(mat), as.integer(dim(mat)[1]), as.integer(dim(mat)[2]), res = as.double(0))$res
}

# Testen der Funktionen
test <- function() {
  m <- matrix(1:9, 3, 3)
  n <- matrix(seq(5, 500, by=5), 10, 10)
  o <- matrix(seq(1, 25000000, by=1), 5000, 5000)
  p <- matrix(seq(1, 64000000, by=1), 8000, 8000)

  # R Code Tests
  t1 <- Sys.time()
  s11 <- sum1(m)
  t2 <- Sys.time()
  s12 <- sum1(n)
  t3 <- Sys.time()
  s13 <- sum1(o)
  t4 <- Sys.time()
  s14 <- sum1(p)
  t5 <- Sys.time()
  cat("R time 1 (small): ", t2-t1, "\n")
  cat("R time 2 (medium): ", t3-t2, "\n")
  cat("R time 3 (big): ", t4-t3, "\n")
  cat("R time 4 (realy big): ", t5-t4, "\n")

  # C Code Tests
  t1 <- Sys.time()
  s21 <- sum2(m)
  t2 <- Sys.time()
  s22 <- sum2(n)
  t3 <- Sys.time()
  s23 <- sum2(o)
  t4 <- Sys.time()
  s24 <- sum2(p)
  t5 <- Sys.time()
  cat("C time 1 (small): ", t2-t1, "\n")
  cat("C time 2 (medium): ", t3-t2, "\n")
  cat("C time 3 (big): ", t4-t3, "\n")
  cat("C time 4 (realy big): ", t5-t4, "\n")
}

test()