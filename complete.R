
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


complete <- function(directory, id = 1:332) {
  
  #completeDataframe <- data.frame ( id = "1" , nobs = "1")
  #names(completeDataframe) <- c("id", "nobs")
  counter = 1
  
  for (i in id){
        ## Openfile to read into a dataframe
        fileName <- paste(directory, "/", formatC(i, width=3, flag="0"), ".csv", sep="")
        fileDataFrame <- read.csv(fileName, header = TRUE)
        
        fileDataFrame <- na.omit(fileDataFrame)
        fileRows <- nrow(fileDataFrame)
        if (counter == 1) {
              completeDataframe <- data.frame ( id = i , nobs = fileRows)
              rownames(completeDataframe) <- counter
        }
        else {
        
          tempDF <- data.frame( id = i , nobs = fileRows)
          rownames(tempDF) <- counter
          completeDataframe <- rbind(completeDataframe , tempDF)
          #completeDataframe[completeRow , 1] <- i
          #completeDataframe[completeRow , 2] <- fileRows
          
          #completeDataframe[completeMatrixRow,2] <- fileRows
          
        }
        counter <- counter + 1
        
  }
  return(completeDataframe)
  
}
