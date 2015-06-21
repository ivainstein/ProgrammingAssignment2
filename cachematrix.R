## Program: cachematrix.R
## Created: June 20, 2015
##
## These two functions: makeCacheMatrix and cacheSolve 
##    work together to calculate the inverse of a matrix.
##
## If the inverse has been previously calculated, the result is taken from a cached (stored) value
##    otherwise the new inverse value calculated and stored.
##    The purpose of the cache is to save time in the calculation of repeated values.

## Function: makeCacheMatrix - This fun0ction creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the storedmatrix value
  storedmatrix <- NULL
  
  ## Initialize the variables
  set <- function(y) {
    x  <<- y
    storedmatrix <<- NULL
    newmatrix <<- NULL
    inversematrix <<- NULL
  }
  
  ## The values of the matrix stored in "x" are assigned to the "get" field
  get <- function() x
  
  ## Functions called to store the inverse and the matrix
  ## into the "inversematrix" and "storedmatrix" variables
  ## done by calling the functions assigned to "setinverse" and "setstored"
  setinverse <- function(inverse) inversematrix <<- inverse
  setstored <- function(newmatrix) storedmatrix <<- newmatrix
  
  ## Functions called to get the stored values of the inverse and 
  ## the matrix that were previously calculated and stored into the
  ## into the "inversematrix" and "storedmatrix" variables
  ## done by calling the functions "getinversematrix" and "getstoredmatrix"
  getinversematrix <- function() inversematrix
  getstoredmatrix <- function() storedmatrix
  
  ## This list of functions makes them availabe in the CacheSolve program
  list(set = set, get = get,
       getstoredmatrix = getstoredmatrix,
       getinversematrix = getinversematrix,
       setstored = setstored,
       setinverse = setinverse)
}


## Function: cacheSolve - This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 

cacheSolve <- function(x, ...) {
  
  m <- x$get()
  m2 <- x$getstoredmatrix()
  
  ## Determine if the matrix' inverse was previouly calculated already
  ## compare the values of the new matrix in "m" to the stored matrix in "m2"
  
  if(!is.null(m) & !is.null(m2) ) {
    
    
    ## Perform a one-to-one comparison on all the cells in the matrices "m" and "m2"
    ## and store the result of the TRUE/FALSE comparison into a matrix "z".
    ## Then, force the values of "z" into a numeric vector and sum the results of the comparison.
    ## Comparisons result in TRUE/FALSE and have assigned the values of 1/0 respectively.
    
    z<-as.vector(m==m2)
    as(z,"numeric")
    sum(z)
    
    ## check that the total number of objects in the matrix equal the 
    ## number of positive comparisons 
    ## if they are the same number, the matrices have been previously solved
    
    if((nrow(m)*ncol(m)) == sum(z)) {
      message("Compared the matrix to the one previously stored")
      message("This matrix was previouly solved - getting the inverse from the cached data")
      
      ## Get the stored value of the inverse by calling the "getinversematrix()" function
      inverse <- x$getinversematrix()
      inverse
      
    }
    
  }
  ## Otherwise, the matrix has not been solved previously.
  ## Calculate the inverse of the matrix with the "solve()" function and store it into the inverse object
  ## store the value of the new matrix by calling the "setstored()" function
  ## store the value of the inverse by calling the "setinverse()" function
  
  x$setstored(m)
  inverse <- solve(m)
  x$setinverse(inverse)
  inverse 
  
}

## Initialize with source("cachematrix.R")
## Test the program like this in this order
## source("cachematrix.R")
## x <- makeCacheMatrix(matrix(c(1,-1,1,2,-2,0,4,4,-4),nrow=3,ncol=3))
## cacheSolve(x)

## Another test for the program like this in this order
## source("makeCacheProgram.R")
## x <- makeCacheMatrix(matrix(c(7,0,-3,2,3,4,1,-1,-2),nrow=3,ncol=3))
## cacheSolve(x)

## You can confirm that they are inverses by doing ordinary matrix multiplication: round(M%*%Minv,3)
