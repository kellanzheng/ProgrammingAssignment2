## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## This script is a pair of functions that cache the inverse of a matrix

## The following function creates a special "matrix" object that is a list that
## contains a function to:
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the value of the inverse
## (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

  x_inv <- NULL
  set <- function(y) {
      x <<- y
      x_inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inv) x_inv <<- inv
  getinv <- function() x_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if
## the inverse  has already been calculated. If so, it gets the inverse
## from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value
## of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  x_inv <- x$getinv()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$setinv(x_inv)
  

}
