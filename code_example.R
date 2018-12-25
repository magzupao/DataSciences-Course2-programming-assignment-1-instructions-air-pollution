#####################################################################  
# Data Science Specialization  
# Marco Guado Zavaleta, mguado@gmail.com  
# Octubre 2016  
# Course 2:  
# programming-assignment-1-instructions-air-pollution  
# function  
##################################################################### 

folderData <- "data"

# descargamos el archivo solo una vez.
if( !file.exists(folderData) ){  
  url = "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip"
  download.file(url, "urlFileData.zip") 
  unzip("urlFileData.zip", exdir = "data")
}

file.temp = read.csv(file="data/specdata/001.csv", header = TRUE, sep = ",")
head(file.temp)

pollutantmean <- function(path.files, pollutant, id = 0) {
  
  # Almacenamos todos los nombres de los archivos csv.
  list.filenames <- list.files(path.files, pattern="*.csv")
  
  # creamos un vector numerico
  list.values <- numeric()
  
  # creamos una validacion si se ingresa el valor 0
  if(length(id) == 1){
    if(id == 0){
      id = 332
    }
  }
  
  # creamos un bucle para recorrer los valores de 1:X
  for(i in c(id) ) {
    
    # procesamos los archivos
    # concatenamos path.files/list.filenames[i]
    filename <- paste(path.files, list.filenames[i], sep = "/")
    
    # cargamos el archivo
    df.File <- read.csv(filename, head=TRUE, sep=",")
    
    # obtenemos los valores de sulfato or nitrato
    if(pollutant == "sulfate"){
      valores <- df.File$sulfate[!is.na(df.File$sulfate)]
    }
    else{
      valores <- df.File$nitrate[!is.na(df.File$nitrate)]
    }
    
    # los valores de cada registro se van acumulando
    list.values <- append(list.values, valores)
  }
  
  # se calcula la media
  return(mean(list.values))
}

# Ejemplo, calcular la media de contaminacion de sulfato para los archivos
# desde el 001.csv hasta 010.csv
pollutantmean ("data/specdata", "sulfate", 1:10)
