## These functions are to cache the inverse of a matrix. 
## We assume that the matrix supplied is always invertible.

## Function makeCacheMatrix creates a "vector", 
## which is a list containing a function to set and get the matrix 
## and to set and get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function cacheSolve calculates the inverse matrix of "vector" from above function.
## First, it checks if the inverse matrix has been solved.
## If yes, it gets the inverse matrix and stop the calculations.
## If no, it solves the inverse matrix and sets it in the cache via setsolve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}
