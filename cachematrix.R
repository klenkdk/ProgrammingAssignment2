## Put comments here that give an overall description of what your
## functions do

## Creates matrix that includes a list of four functions
## which can get/set the data as well as the inverse of
## data.  The inv is set in the global environment and
## can be accessed (cached) without recomputing.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(s) inv <<- s
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'.
## Accessses cached inverse if available in global
## environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
