## In linear algebra, an n-by-n square matrix A is called invertible 
## if there exists an n-by-n square matrix B, which is the inverse of A, such that
## AB = BA = I_n
## where I_n denotes the n-by-n identity matrix and the multiplication used is ordinary matrix multiplication

## makeCacheMatrix function sets the values of the Inverse matrix to be cached for saving future computations
## Consists of 4 functions - 
## - getting and setting the original matrix
## - getting and setting the inverse of the matrix
## Assumptions - the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
inverseMatrix <- NULL

## set function
  set <- function (y) {
      x <<- y
      inverseMatrix <<- NULL
  }
## get function
  get <- function() x

## setInverse function
  setInverse <- function (iMatrix)
      inverseMatrix <<- iMatrix

## getInverse function
  getInverse <- function() inverseMatrix

## Making a list of the functions in makeCacheMatrix
list (set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Computation of the Inverse of an Invertible matrix and caching it for future computations 
## cacheSolve returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
   iMatrix <- x$getInverse()
   if(!is.null(iMatrix)) {
        message("Getting cached matrix inverse") 
        return(iMatrix)  
   }
   
   ## Solving for inverse of the passed matrix
   iMatrix <- solve(x$get())
   x$setInverse(iMatrix)
   iMatrix
}
