#####################################################################
# Data Science Specialization
# Marco Guado Zavaleta, mguado@gmail.com
# Octubre 2016
# Course 2:
# programming-assignment-1-instructions-air-pollution
#####################################################################

# We download the data to be processed.
url = "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip"
download.file(url, "urlFileData.zip") 
unzip("urlFileData.zip", exdir = "data") 

list.files()

# We scanned the contents of the files. Let's take as an example the reading of the first file.
file.temp = read.csv(file="data/specdata/001.csv", header = TRUE, sep = ",")
head(file.temp)

# Function created:
pollutantmean <- function(path.files, pollutant, id = 0) {
  
  #We store all file names
  list.filenames <- list.files(path.files, pattern="*.csv")
  
  #numercia list that stores all values, useful to determine the median
  list.values <- numeric()
  
  #this validation is useful if you enter as parameter id = 0
  if(length(id) == 1){
    if(id == 0){
      id = 332
    }
  }
  
  for(i in c(id) ) {
    
    #file name processing
    filename <- paste(path.files, list.filenames[i], sep = "/")
    
    #load the file contents
    df.File <- read.csv(filename, head=TRUE, sep=",")
    
    #filter by sulfate or nitrate
    if(pollutant == "sulfate"){
      valores <- df.File$sulfate[!is.na(df.File$sulfate)]
    }
    else{
      valores <- df.File$nitrate[!is.na(df.File$nitrate)]
    }
    
    #It contains all values for each interaction
    list.values <- append(list.values, valores)
  }
  
  #calculation of the mean
  return(mean(list.values))
}

# Homework 1
# We execute the function to calculate the average summing all the files from 1 to the last 332.
pollutantmean ("specdata", "sulfate", 1:332)

# Homework 2
# Make a sum for each day observed of the variables.
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

# Example
tmp <- complete ("specdata", 1:10)
tmp

# Homework 3
# The correlation coefficient provides a measure of how two random variables are associated in a “sample”.
tmp.cor <- cor(tmp)
tmp.cor

plot(tmp$nobs.sulfate, tmp$nobs.nitrate)

#The correlation between the two variables is strong, if one variable grows the other also grows.
#Resumen
#We can affirm that at higher levels of asufre contamination there are also higher levels of nitrate.