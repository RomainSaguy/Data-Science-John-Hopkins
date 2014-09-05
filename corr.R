corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
        ## Création d'un vecteur comprenant l'ensemble des noms de fichiers .csv à aller chercher
        
        n <- rep(NA, times = 332)
        
        for (i in 1:332) {
                if (i < 10) { 
                        n[i] <- paste("00", i, ".csv", sep = "")
                } else if (i >= 10 & i < 100 ) {
                        n[i] <- paste("0", i, ".csv", sep = "")
                } else {
                        n[i] <- paste(i, ".csv", sep = "")
                }
        }
        
        ## Intégration des valeurs des polluants de chaque fichier au sein d'une matrice
        
        cr <- rep(NA, times = 332)
        
        
        for (i in 1:332) {
                t <- read.csv(paste("./", directory, "/", n[i], sep = "")) 
                
                if (sum(!is.na(t[,2]) & !is.na(t[,3])) > threshold) {
                        cr[i] <- cor(t$sulfate, t$nitrate, use = "complete.obs")
                } else {
                        cr[i] <- NA
                }
        }
        
       cr <- cr[!is.na(cr)]
}