complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
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
        
        x <- as.data.frame(matrix(data = NA, ncol = 2, nrow = length(id)))
        names(x) <- c("id", "nobs")
        
        for (i in 1:length(id)) {
                t <- read.csv(paste("./", directory, "/", n[i], sep = ""))  
                x[i,2] <- sum(!is.na(t[,2]) & !is.na(t[,3]))
                x[i,1] <- t$ID[1]     
                } 
        
        print(x)
}