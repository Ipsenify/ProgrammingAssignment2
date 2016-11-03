## Sam Ipsen
## Programming Assignment 2: Lexical Scoping
## 11/3/2016
## Creates and manages a Matrix with a cache for its Inverse


## Creates a 'special' matrix with getters and setters for
## the internal matrix and its cached inverse
makeCacheMatrix <- function(x = matrix()) {
  inv_cache <- NULL
  set <- function(y) {
    x <<- y
    inv_cache <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inv_cache <<- inv
  getInverse <- function() inv_cache
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns the inverse of the provide matrix
## If the cache is set, it will return the cache
## otherwise it will calculate the inverse, set the cache 
## and return the result
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
