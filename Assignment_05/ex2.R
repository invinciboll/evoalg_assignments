# gleiche population, 100 Generationen

# Jede Generation wird wie folgt erzeugt:
# 1. Elternseketion mit SUS Sampling
# 2. Erzeugung Kind durch clonen eines Elternteils
# 3. Generationenmodell: Alle erzeugten Kinder ersetzten Eltern
# => 2 und 3 können zusammengefasst werden in die gewählten Eltern 
# kommen direkt in die neue Population

# checking which value in the wheel this value corresponds to
check_for_corresponding_vector <- function(selection_vector, val) {
    found <- FALSE
    j <- 1
    check_sum <- 0
    while (!found) {
        check_sum <- check_sum + selection_vector[j]
        if (check_sum > val) {
            found <- TRUE
        } else {
            j <- j + 1
        }
    }
    return(j)
}

sus <- function(pop) {
    new_pop <- c()
    # erstellen des fitness proportion vektors
    m <- mean(pop)
    # m * length(pop) damit die Summe 1 ist
    selection <- pop / m #/ (m * length(pop))
    s <- sum(selection)

    # i unserem Fall ist die Anzahl der Picks die Groesse der Population
    picks <- length(pop)
    step_size <- s / picks
    r <- runif(1, 0, 1 / length(pop))

    # in unserem Fall wissen wir wie oft wir ziehen und dass gleichmäßig
    # aufgeteilt ist, daher benötigen wir keine while Schleife
    for (i in 0:(picks - 1)) {
        # current value
        val <- r + i * step_size
        val_index <- check_for_corresponding_vector(selection, val)
        # clonen des Elternteils als neues Kind
        child <- pop[val_index]
        # Generationenmodell
        new_pop <- append(new_pop, child)
    }
    return(new_pop)
}

run <- function() {
    pop <- runif(10)
    cat("Inital Pop: \n")
    print(pop)
    cat("\n")

    for (i in seq_len(100)) {
        pop <- sus(pop)
        cat("Generation: ", i, "\n")
        print(pop)
        cat("\n")
    }
}

run()