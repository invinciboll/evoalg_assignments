# 20 zweidimensionale Individuen
# - alle Komponenten werden zufällig aus {0,1} gewählt
# Zufällige Elternselection
# Erzeugung von 2 Kindindividuen durch Rekombination des Elternpaares
# Generationenmodell
# 10 Generationen

# Single Arithmetic Recombination (1.alpha = 0.5 und 2. element[0,1] für jedes Elternpaar)
# Whole Arithemtic Recombination (1.alpha = 0.5 und 2. element[0,1] für jedes Elternpaar)

# Visualisierung der Ergebnisse
plt <- function(pop, recombination_method, alpha_rand, i, method) {
    if (method == "sar") {
        if (alpha_rand) {
            name <- paste("Single AR - alpha is random - Generation: ", i)
        } else {
            name <- paste("Single AR - alpha = 0.5 - Generation: ", i)
        }
    } else {
        if (alpha_rand) {
            name <- paste("Whole AR - alpha is random - Generation: ", i)
        } else {
            name <- paste("Whole AR - alpha = 0.5 - Generation: ", i)
        }
    }

    # formating for ploting
    x <- c()
    y <- c()
    for (i in seq_len(length(unlist(pop)))) {
        if (i %% 2 == 0) {
            x <- append(x, unlist(pop)[i])
        } else {
            y <- append(y, unlist(pop)[i])
        }
    }

    plot(x, y, main = name, xlab = "x", ylab = "y", type = "p", cex = 0.5, col = "blue")
    hpts <- chull(x, y)
    hpts <- c(hpts, hpts[1])
    lines(x[hpts], y[hpts])
}

# Population Initialisierung
init <- function() {
    pop <- list()
    for (i in seq_len(20)) {
        pop[[i]] <- runif(2, 0, 1)
    }
    return(pop)
}

# Single Arithmetic Recombination
single_ar <- function(indiv1, indiv2, alpha_rand) {
    children <- list()
    alpha <- 0.5
    if (alpha_rand) {
        alpha <- runif(1)
    }
    # pick single gene
    k <- sample(0:1, 1)
    indiv1[k] <- alpha * indiv2[k] + (1 - alpha) * indiv1[k]
    indiv2[k] <- alpha * indiv1[k] + (1 - alpha) * indiv2[k]

    children[[1]] <- indiv1
    children[[2]] <- indiv2
    return(children)
}

# Whole Arithmetic Recombination
# takes weighted sum of alleles from parents
whole_ar <- function(indiv1, indiv2, alpha_rand) {
    children <- list()
    i1 <- c(0.0, 0.0)
    i2 <- c(0.0, 0.0)
    alpha <- 0.5
    if (alpha_rand) {
        alpha <- runif(1)
    }

    # Berechnung Kind 1
    i1[1] <- alpha * indiv1[1] + (1 - alpha) * indiv2[1]
    i1[2] <- alpha * indiv1[2] + (1 - alpha) * indiv2[2]
    # Berechnung Kind 2
    i2[1] <- alpha * indiv2[1] + (1 - alpha) * indiv1[1]
    i2[2] <- alpha * indiv2[2] + (1 - alpha) * indiv1[2]

    children[[1]] <- i1
    children[[2]] <- i2
    return(children)
}

# Algorithmus fuer eine Generation
gen <- function(pop, recombination_method, alpha_rand, method) {
    new_pop <- list()
    i <- 1
    while (length(pop) >= 2) {
        # Selektion
        indiv1 <- sample(pop, 1, replace = FALSE)
        pop[[which(pop %in% indiv1)[1]]] <- NULL
        indiv2 <- sample(pop, 1, replace = FALSE)
        pop[[which(pop %in% indiv2)[1]]] <- NULL
        # Rekombination
        children <- recombination_method(indiv1[[1]], indiv2[[1]], alpha_rand)
        # Generationenmodell - nur Kinder werden übernommmen
        new_pop[[i]] <- children[[1]]
        new_pop[[i + 1]] <- children[[2]]
        i <- i + 2
    }
    return(new_pop)
}

# Algorithm for all generations
ga <- function(recombination_method, alpha_rand = FALSE, name, method) {
    print(name)
    pdf(file = name)
    pop <- init()
    # 10 Generationen
    for (i in seq_len(10)) {
        pop <- gen(pop, recombination_method, alpha_rand, method)
        plt(pop, recombination_method, alpha_rand, i, method)
    }
    dev.off()
    return(pop)
}


# Berechnen des Algorithmus für verschiedene Werte
main <- function() {
    # Single Arithmetic Recombination
    ga(single_ar, name = "sar-5.pdf", method = "sar")
    ga(single_ar, alpha_rand = TRUE, name = "sar-random.pdf", method = "sar")

    # Whole Arithmetic Recombination
    ga(whole_ar, name = "war-5.pdf", method = "war")
    ga(whole_ar, alpha_rand = TRUE, name = "war-random.pdf", method = "war")
}

main()