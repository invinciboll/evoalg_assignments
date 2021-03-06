# Genetischer Algorithmus (GA) f�r bin�re zenhdimensionale Datenpunkte.

# v(10) = individuum
# 2 individuen sollen zuf�llig erzeugt werden.
# In jeder generation werden Eltern durch Kinder vollst�ndig ersetzt

# crossover - recombination operators - mutation = bit-flipping with fixed (low) probability
# recombination: N-point or uniform crossover

# 3 Crossover Mechanismen:
# 1-point crossover
# 2-point crossover
# Uniform crossover

# Allgemeiner Ablauf eines GA:
# 1. SELECT parents (-mating select)
# 2. RECOMBINE - Crossover mechanismen
# 3. MUTATE - haben wir nicht
# 4. EVALUATE
# 5. SELECT - Wir nehmen immer die beiden Kinder, welche wir erstellt haben


# 1-point crossover
pco1 <- function(pop) {
  # wahrscheinlich kann cop nicht an der letzten Stelle sein, da sonst kein crossover wirklich stattfinden kann
  cop <- sample(1:(length(pop[[1]])-1),1)
  frontSplits <- list()
  backSplits <- list()
  
  # splitten der gene in 2 listen
  for(i in 1:length(pop)) {
    indexF <- length(frontSplits)+1
    indexS <- length(backSplits)+1
    frontSplits[[indexF]] <- pop[[i]][1:cop]
    backSplits[[indexS]] <- pop[[i]][cop+1:(length(pop[[i]])-cop)]
  }
  
  # zusammenf�hren der gene
  child1 <- c(frontSplits[[1]], backSplits[[2]])
  child2 <- c(frontSplits[[2]], backSplits[[1]])
  
  # ersetzen der Elternpopulation
  pop[[1]] <- child1
  pop[[2]] <- child2
  
  return(pop)
}

# 2-point crossover
pco2 <- function(pop) {
  # Choose 2 cop elements 
  cop1 <- sample(1:(length(pop[[1]])-1),1)
  cop2 <- sample(1:(length(pop[[1]])-1),1)
  
  # falls das gleiche Element gew�hlt wurde
  if(cop1 == cop2) {
    if(cop1 == 1) {
      cop2 <- cop2 + 1
    } else {
      cop1 <- cop1 - 1
    }
  }

  if(cop1 > cop2) {
    # vertausche variablen
    cop1 <- cop1 + cop2
    cop2 <- cop1 - cop2
    cop1 <- cop1 - cop2
  }

  frontSplits <- list()
  middleSplits <- list()
  backSplits <- list()
  
  # splitten am ersten und zweiten cop
  for(i in 1:length(pop)) {
    indexF <- length(frontSplits)+1
    indexM <- length(middleSplits)+1
    indexS <- length(backSplits)+1
    
    frontSplits[[indexF]] <- pop[[i]][1:cop1]
    middleSplits[[indexM]] <- pop[[i]][(cop1+1):cop2]
    backSplits[[indexS]] <- pop[[i]][(cop2+1):length(pop[[i]])]
  }
  
  # zusammenf�hren der gene
  child1 <- c(frontSplits[[1]], middleSplits[[2]], backSplits[[1]])
  child2 <- c(frontSplits[[2]], middleSplits[[1]], backSplits[[2]])
  
  # ersetzen der Elternpopulation
  pop[[1]] <- child1
  pop[[2]] <- child2
  
  return <- pop
}

# uniform crossover
uniform <- function(pop) {
  child <- c()
  # laufe einmal durch und immer 50% chance, dass entweder das Bit des einen oder anderen ausgew�hlt wird
  # wenn ein Gen komplett erstellt wird erstelle das inverse Element
  for(i in 1:length(pop[[1]])) {
    if(sample(0:1,1) == 1) {
      # w�hle Gen 1
      child[i] <- pop[[1]][i]
    } else {
      # w�hle Gen 2
      child[i] <- pop[[2]][i]
    }
  }
  
  pop[[1]] <- child
  # inverse des Kindes
  inv_child <- c()
  for(i in 1:length(child)) {
    if(child[i] == 0) {
      inv_child[i] <- 1
    } else {
      inv_child[i] <- 0
    }
  }
  pop[[2]] <- inv_child
  
  return(pop)
}

run <- function() {
  
  crossoverTypes <- c(pco1, pco2, uniform)
  crossoverTypeName <- c("1-point crossover", "2-point crossover", "uniform crossover")

  # runing for all 3 kinds of crossovers
  for(i in 1:length(crossoverTypes)) {

    #* creating start vectors
    pop <- list(sample(0:1, 10, replace=TRUE), sample(0:1, 10, replace=TRUE))
    
    # Ausgabe Startvector
    cat("Crossovertype: ", crossoverTypeName[i], "\n")
    cat("Startpopulation: \n")
    print(pop)
    
    # 10.000 Generations = 10.000 itterations
    for(j in 1:10000) {
      # print(crossoverTypes[[i])
      pop <- crossoverTypes[[i]](pop)
    }
    
    # Ausgabe
    cat("Endpop: \n")
    print(pop)
  }
}

run()

# Erzeugen Sie 10.000 Generationen. Wie viele verschiedene Lösungen können Sie im Verlauf des Experiments beobachten und wie viele der insgesamt 2^10 möglichen Lösungen werden hierbei abgedeckt?

# Bei den 1-point crossover und 2-point crossover types haben sich Anfangsmatches, also die ersten n Zeichen beider Gene, welche identisch sind, nicht vermischt. 
# Ein Beispiel dazu ist, dass Gen1 und Gen2 mit 1,1 gestartet sind und aber �ber alle Generationen hat sich dieses Verhalten nicht ver�ndert, sogar in der 
# Endpopulation sind in beiden Genen die 1,1 am Anfang weiterhin vorhanden. Falls die Gene also am sehr identische Anf�nge haben schr�nkt dies den Faktor f�r 
# Ver�nderungen der Gene stark ein, da sich dieser Teil des Gens nicht ver�ndert.
# Dieses Verhalten ist bei dem Uniform crossover nicht zu beobachten und das verhalten bei uniform crossover scheint grunds�tzlich wesentlich flexibler.

# In diesem Fall, in dem die ersten zwei Stellen sich nicht ver�nderen lassen sind von den 2^10 m�glichen Mutationen zu 2^8 Mutationen geschrumpft. 
# Bei mehr n identischen Anfangscharactern reduziert sich die Rate auf 2^(10-n) bei einem Gen der L�nge 10.
# Man m�chte also versuchen, dass die Startgene Am Anfang m�glichst verschieden sind. 
# Im Falle des uniform crossover wird im Idealfall in jeder Generation ein neues Gen entdeckt und dann das inverse Gen erstellt. Man entdeckt also in jeder 
# Generation 2 neue Gene. In 10.000 Generationen entdeckt man also 20.000 neue Gene.
# Allerdings haben die entdeckten Kinder eine Abh�nigkeit darin, dass das Zweite immer das Inverse zum Ersten ist und demnach werden nicht beliebige neuen Gene
# entdeckt sondern 10.000 "Paare" bzw. paarweise verschiedene Gene. 
