#####################################################################  
# Data Science Specialization  
# Marco Guado Zavaleta, mguado@gmail.com  
# Octubre 2016  
# Course 2:  
# programming-assignment-1-instructions-air-pollution  
# function  
#####################################################################  
  
pollutantmean <- function(path.files, pollutant, id = 0) {
  
  #We store all file names
  list.filenames <- list.files(path.files, pattern="*.csv")
  
  #numercia list that stores all values, useful to determine the median
  list.values <- numeric()
  
  # this validation is useful if you enter as parameter id = 0
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
