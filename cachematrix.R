## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
##  Functions to inverse a matrix
##  Usage:
##  > source ("cachingMatrixInverse.R")
##  > m <- makeCacheMatrix (matrix (c(1,2,3,4), nrow=2, ncol=2))
##  > cacheSolve(m)
##
##         [,1] [,2]
##   [1,]   -2  1.5
##   [2,]    1 -0.5
##
##  Function: makeCacheMatrix
##            Construct the matrix
##           Set Matrix, Get Matrix, Set Inverse of Matrix, Get Inverse of Matrix
##
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL 
  
  setMatrix <- function(inputMatrix) 
  { 
    x <<- inputMatrix 
    inverseMatrix <<- NULL 
  } 
  getMatrix <- function() x 
  setInverseMatrix <- function(inputInverseMatrix) inverseMatrix <<- inputInverseMatrix 
  getInverseMatrix <- function() inverseMatrix 
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix) 
}


## Write a short comment describing this function
##  Function: cacheSolve
##            Inverse the matrix using solve function
##            Cache it. Use it if already available
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverseMatrix() 
  
  if(!is.null(inverseMatrix))
  { 
    message("getting cached data") 
    return(inverseMatrix) 
  } 
  newMatrix <- x$getMatrix() 
  inverseMatrix <- solve(newMatrix, ...) 
  x$setInverseMatrix(inverseMatrix) 
  inverseMatrix 
}

