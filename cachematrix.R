## Functions to store, retrieve, and calculate the inverse of a matrix.
## By caching the inverse matrix, calculation time is saved if the inverse is used repeatedly

## makeCacheMatrix stores a matrix, and provides the functions to change the matrix, get the inverse matrix stored
## in the cache, and also change the inverse matrix stored in the cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Gets inverse matrix stored in cache. If inverse matrix already cached, returns cached inverse.
## If nothing in cache, solves inverse and stores in cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()       ##gets the matrix from MakeCacheMatrix()
  m <- solve(data, ...)  ##computes inverse matrix
  x$setinverse(m)       ##sets cache to computed inverse
  m
}
