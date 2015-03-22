
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations

corr <- function(directory, threshold = 0) {
  corrVector <- vector("numeric")
  corVectorDis <- vector("numeric")
  
  completeMatrix <- complete(directory , 1:332)
  
  for (i in 1:332){
   
    if (completeMatrix[i,2] > threshold){
      
      corrVector <- c(corrVector, completeMatrix[i,1] )
      }
  } 
  if (length(corrVector) != 0){
          
        # Vector of suitable monitors # print (corrVector)
        for (i in 1:length(corrVector)){
          fileName <- paste(directory, "/", formatC(corrVector[i], width=3, flag="0"), ".csv", sep="")
          fileDataFrame <- read.csv(fileName, header = TRUE)
          
          fileDataFrame <- na.omit(fileDataFrame)
          
          ## Extracting a vector from the dataFrame and appending to the end of the numeric vector
          
          corValueMonitor <- cor(fileDataFrame[[2]] , fileDataFrame[[3]])
          corVectorDis <- c(corVectorDis,corValueMonitor)
        }
  }    
  
  return(corVectorDis)
}