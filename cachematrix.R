## This pair of functions (1.) defines a cache and
## a matrix-like object whose functions define
## its underlying properties, then (2.) checks
## to see whether the matrix has already been
## inverted, before either inverting or returning
## the cached inverse.

## makeCacheMatrix constructs a "matrix" object containing
## method-like functions, from the matrix provided (x)

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setinv <- function(inv) cache <<- inv
  getinv <- function() cache
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve checks to see if there is a cached inverse of x; if not,
## it computes the inverse of x, caches it, and returns that matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
