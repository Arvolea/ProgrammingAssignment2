## Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix 
## inversion that we will not discuss here). Your assignment is to write a pair of functions that 
## cache the inverse of a matrix.

## https://github.com/Arvolea/ProgrammingAssignment2

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(M1 = matrix()) 
{
  invM <- NULL
  set <- function(M2) 
  {
    M1 <<- M2
    invM <<- NULL
  }
  get <- function() M1
  
  setinvM <- function(inverse) invM <<- inverse
  
  getinvM <- function() invM
 
  list(set=set, get=get, setinvM=setinvM, getinvM=getinvM)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache

cacheSolve <- function(M, ...) 
{
  invM <- M$getinvM()
  if(!is.null(invM)) 
  {
    message("Retriving Matrix from the cached data.")
    return(invM)
  }
  data <- M$get()
  
  invM <- solve(data)
  
  M$setinvM(invM)
  
  invM
}

## Sample and Results

## > M1 <- rbind(c(1,1/6), c(1/6,1))
## > M2 <- makeCacheMatrix(M1)
## > M2$get()

## [,1]      [,2]
## [1,] 1.0000000 0.1666667
## [2,] 0.1666667 1.0000000


## > cacheSolve(M2)
## [,1]       [,2]
## [1,]  1.0285714 -0.1714286
## [2,] -0.1714286  1.0285714

## > cacheSolve(M2)
## Retriving Matrix from the cached data.
## [,1]       [,2]
## [1,]  1.0285714 -0.1714286
## [2,] -0.1714286  1.0285714