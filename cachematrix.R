## The below functions are used to cache the inverse of a matrix. 
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly

## This funcion creates a special 'matrix' opbject that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvert <- function(solve) m <<- solve
  getinvert <- function() m
  list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)
  }


## This function computes the inverse of the special 'matrix' retuned by makeCaheMatrix.  If the inverse
## has already been calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinvert()
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvert(m)
  m

}
