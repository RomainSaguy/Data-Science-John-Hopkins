
## Instructions : Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a 
## specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
## Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory 
## specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values 
## coded as NA.

directory <- "specdata"
pollutant <- "nitrate"
id <- 70:72

pollutantmean <- function(directory, pollutant, id = NA) {
        
if (is.na(id) == TRUE) {
        id <- 1:332
        } 
        
        ## Création d'un vecteur comprenant l'ensemble des noms de fichiers .csv à aller chercher
        
        n <- rep(NA, times = length(id))
        
        for (i in 1:length(id)) {
                if (id[i] < 10) { 
                        n[i] <- paste("00", id[i], ".csv", sep = "")
                } else if (id[i] >= 10 & id[i] < 100 ) {
                        n[i] <- paste("0", id[i], ".csv", sep = "")
                } else {
                        n[i] <- paste(id[i], ".csv", sep = "")
                }
        }
        
        ## Intégration des valeurs des polluants de chaque fichier au sein d'une matrice
        
        x <- matrix(data = NA, ncol = length(id), nrow = 10000)
                
        for (i in 1:length(id)) {
                t <- read.csv(paste("./", directory, "/", n[i], sep = ""))
                
                if (pollutant == "sulfate") {
                        col <- 2
                } else if (pollutant == "nitrate") {
                        col <- 3
                        }
                
                z <- i
                
                for (i in 1:nrow(t)) {
                        x[i,z] <- t[i,col]
                }                
        }
        
        ## Création d'un vecteur rassemblant l'ensemble des valeurs de la matrice précédente
        
        sum <- rep(NA, length(id))
        nbr <- rep(NA, length(id))
        
        for (i in 1:length(id)) {
                sum[i] <- sum(x[,i], na.rm = TRUE)
                nbr[i] <- sum(!is.na(x[,i]))
        }
        
        mean <- sum(sum) / sum(nbr)
        
        mean
}
