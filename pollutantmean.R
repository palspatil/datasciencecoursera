## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)
pollutantmean <- function(directory, pollutant, id = 1:332) {

pollutantMeanVector <- vector("numeric")
  
  for (i in id){
    ## Form the colClasses vector to limit file read
    colClasses=c(rep("NULL",4))
    if (identical(pollutant,"sulfate")){
      col = 2
      colClassString = c("NULL", "numeric", rep("NULL",2))
    }
    else if (identical(pollutant, "nitrate")){
      col = 3
      colClassString = c(rep("NULL",2), "numeric", "NULL")
    }
    else
      col = 0
    
    ## print(c("Pollutant = ",col))
    ## print(colClassString)
    
    ## Openfile to read into a dataframe
    fileName <- paste(directory, "/", formatC(i, width=3, flag="0"), ".csv", sep="")
    ## print(fileName)
    fileDataFrame <- read.csv(fileName, header = TRUE, colClasses = colClassString)
    
    ## Extracting a vector from the dataFrame and appending to the end of the numeric vector
    pollutantMeanVector <- c(pollutantMeanVector,fileDataFrame[[1]])
     
  }
  
  print("MEAN of Pollutant across monitors = ")
  print(mean(pollutantMeanVector, na.rm = TRUE))
}

