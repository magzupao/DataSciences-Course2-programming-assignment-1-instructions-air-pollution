corr <- function(path.files, id = 0) {
  
  #We store all file names
  list.filenames <- list.files(path.files, pattern="*.csv")
  
  list.cor <- c()
  
  # this validation is useful if you enter as parameter id = 0
  if(length(id) == 1){
    if(id == 0){
      id = 332
    }
  }
  
  for(i in 1:length(list.filenames)){
    
    #file name processing
    filename <- paste(path.files, list.filenames[i], sep = "/")
    
    #load the file contents
    df.File <- read.csv(filename, head=TRUE, sep=",")  
    #df.File <- df.File %>% drop_na()
    
    if ( nrow(df.File) > id ) {
      list.cor <- c(list.cor, cor(df.File$sulfate, df.File$nitrate))
    }
  }
  
  return(list.cor)
}
#a <- corr("specdata", 1:10)