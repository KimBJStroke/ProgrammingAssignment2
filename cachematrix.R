## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## 1. makeCacheMatrix: 
### This function creates a special "matrix" object that can cache its inverse.

## 2. cacheSolve: 
### This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
### If the inverse has already been calculated (and the matrix has not changed), 
### then the cachesolve should retrieve the inverse from the cache.



makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL # initialize to NULL
  set <- function(y) { # create the matrix in the working environment
    x <<- y
    cache <<- NULL
  }
  get <- function () x # getting the value of the matrix
  setinv <- function(inverse) cache <<- inverse # invert the matrix and store in cache
  getinv <- function() cache # get the inverted matrix from cache
  list (set=set, get=get, setinv = setinv, getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    cache <- x$getinv() # retreive the inverted matrix from stored cache
    if(!is.null(cache)) { # retrun inverted matrix from cache or create the matrix in the working sapce
      message("retrieving cached data.")
      return(cache) # display matrix
    }
    matrix <- x$get() # creating matrix as NULL in cache
    cache <- solve(matrix)
    x$setinv(cache)
    return(cache)
  }



