#####################################################################
# Data Science Specialization
# Marco Guado Zavaleta, mguado@gmail.com
# Octubre 2016
# Course 2:
# programming-assignment-1-instructions-air-pollution
# function
#####################################################################

complete  <- function(path.files, id) {
  
  #We store all file names
  list.filenames <- list.files(path.files, pattern="*.csv")
  
  id.file <- integer()
  num.filas1 <- integer()
  num.filas2 <- integer()
  
  for(i in c(id) ) {
    
    #file name processing
    filename <- paste(path.files, list.filenames[i], sep = "/")
    
    #load the file contents
    df.File <- read.csv(filename, head=TRUE, sep=",")
    
    filas1 <- length(df.File$sulfate[!is.na(df.File$sulfate)])
    filas2 <- length(df.File$nitrate[!is.na(df.File$nitrate)])
    
    id.file <- append(id.file, i)
    num.filas1 <- append(num.filas1, filas1)
    num.filas2 <- append(num.filas2, filas2)
    
  }
  
  rspta <- data.frame(id.file , num.filas1, num.filas2)
  
  #rspta.names <- c("id", "nobs")
  colnames(rspta) <- c("id.file", "nobs.sulfate", "nobs.nitrate")
  
  rspta
  
}
#a <- complete ("specdata", 1:10)