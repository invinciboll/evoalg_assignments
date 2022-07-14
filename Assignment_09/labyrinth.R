##############################################################################################
##
## fitness
##
## Bewertet die Fitness eines Agenten agent anhand der Schritte, die er braucht um ein Labyrinth labyrinth zu durchlaufen

# agent:        Eine Funktion, die die Bewegung in einem Labyrinth simuliert
#               Input:  mat 3x3 Matrix, einen Bildausschnitt des Labyrinths. Der Agent steht auf Position.
#                   "S" = Start (Es existiert nur ein Start)
#                   "E" = Ende  (Es existiert nur ein Ende)
#                   "W" = Wand
#                   "F" = Frei
#               Output: Die Richtung, in die der Agent laufen soll. ("l","r","o","u")

# labyrinth:    n x m Matrix aus characters mit Einträgen
#                   "S" = Start
#                   "E" = Ende
#                   "W" = Wand
#                   "F" = Frei

# k:            Maximal erlaubte Anzahl an Schritten

# Rückgabe:     Benötigte Anzahl Schritte (max k)

##############################################################################################

fitness <- function(agent, labyrinth, k= 1000){
    act.pos  <- which(labyrinth == "S", arr.ind = T)[1,]
    ende     <- which(labyrinth == "E", arr.ind = T)[1,]
    
    i=0
    while(!all(act.pos == ende))
    {
        #Bildausschnitt
        mat <- labyrinth[act.pos["row"]+c(-1,0,+1), act.pos["col"]+c(-1,0,+1)]
        
        #Bewegung
        act.pos <- move(act.pos, agent(mat), labyrinth)
        
        i=i+1
        if(i==k){
            break
        }
    }
    
    return(i)
}

##############################################################################################
##
## move
##
## Bewegt den Agenten von Position pos um ein Feld in Richtung dir.
## Ist eine Wand im Weg, bleibt der Agent auf der Position pos.

# pos:       Koordinaten (x,y)

# labyrinth:    n x m Matrix aus characters mit Einträgen
#                   "S" = Start
#                   "E" = Ende
#                   "W" = Wand
#                   "F" = Frei

# k:            Maximal erlaubte Anzahl an Schritten

move <- function(pos, dir, labyrinth){
    new.pos <- switch(dir,
                "l" = pos+c(0,-1),  #links
                "r" = pos+c(0,+1),  #rechts
                "u" = pos+c(-1,0),  #oben
                "o" = pos+c(+1,0))  #unten
                 
    if(labyrinth[new.pos["row"],new.pos["col"]] == "W"){    # Agent läuft gegen die Wand
        return(pos)
    }else{ # Agent läuft auf freies Feld
        return(new.pos)
    }
}


##############################################################################################
##
## plot.gesamt
##
## Zeichnet die Bewegungen des Agenten

plot.gesamt <- function(agent, labyrinth, k= 1000){
    act.pos  <- which(labyrinth == "S", arr.ind = T)[1,]
    ende     <- which(labyrinth == "E", arr.ind = T)[1,]
    
    plot.lab(labyrinth)
    plot.pos(act.pos)
    i=0
    while(!all(act.pos == ende))
    {
        #Bildausschnitt
        mat <- labyrinth[act.pos["row"]+c(-1,0,+1), act.pos["col"]+c(-1,0,+1)]
       
        #Bewegung
        act.pos <- move(act.pos, agent(mat), labyrinth)
        
        plot.lab(labyrinth)
        plot.pos(act.pos)
        
        i=i+1
        if(i==k){
            break
        }
    }
}

####################

plot.pos <- function(pos){
    rect(   pos[2]-0.5,
            pos[1]-0.5,
            pos[2]+0.5,
            pos[1]+0.5,
            col = "red",
            border = 'black')
}

####################
plot.lab <- function(labyrinth){
    labyrinth[labyrinth=="S"] <- "yellow"
    labyrinth[labyrinth=="E"] <- "green"
    labyrinth[labyrinth=="W"] <- "gray"
    labyrinth[labyrinth=="F"] <- "white"
    
    plot(   0,0,
            type = 'n',
            xlim = c(0.5,ncol(labyrinth)+0.5),
            ylim = c(nrow(labyrinth)+0.5,0.5),
            main = "",
            xlab = "",
            ylab = '',
            xaxt = 'n',
            yaxt = 'n',
            yaxs = 'i',
            xaxs = 'i',
            bty = 'n')
            
    coordinates <- expand.grid(y=1:nrow(labyrinth),x=1:nrow(labyrinth))
    
    rect(   coordinates[,'x']-0.5,
            coordinates[,'y']-0.5,
            coordinates[,'x']+0.5,
            coordinates[,'y']+0.5,
            col = labyrinth,
            border = 'black')
}


#########################################################################
#########################################################################
###
###
### Main

agent.dummy <- function(mat){
    sample(c("r","l","o","u"),1)
}

load("maze01.RData")
#mat

pdf("test.pdf")
plot.gesamt(agent=agent.dummy, labyrinth=mat, k= 1000)
dev.off()
