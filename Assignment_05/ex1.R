# Population aus 10 Individuen mit 0 <= fitness <= 1.
# Individuum wird diret durch FItnesswert charakterisiert.

# Testen verschiedener Transformationen der fitnessfunktion
# f = a * f + b # nolint
a <- c(0, 1, 2, 4, 10)
b <- c(0, 1, 2, 4, 10)

# 1. initial population
# 2. anwenden der transformation
# 3. betrachten der Werte

run <- function() {
    for (i in seq_len(length(a))) {
        cat("\n________________ a = ", a[i], " ________________\n\n")
        for (j in seq_len(length(b))) {
            cat("a: ", a[i], " - b: ", b[j], "\n")
            pop <- runif(10)
            cat("Initial population: \n")
            print(pop)

            pop <- (a[i] * pop) + b[j]
            cat("After scaling \n")
            print(pop)
        }
    }
}

run()