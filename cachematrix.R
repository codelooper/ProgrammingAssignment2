## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function makeCacheMatrix() creates a special "vector", which is really a list containing functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse){inv <- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get,
       setsolve = setInverse,
       getsolve = getInverse)
}
## The function cacheSolve() calculates the inverse of a matrix if receiving data for the first time, otherwise it gets it if it has already been calculated
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

