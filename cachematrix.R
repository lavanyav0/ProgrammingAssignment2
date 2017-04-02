## This programming assignment solution modifies the example provided
## to cache the inverse of a matrix and calculate the inverse when there is no cache 
## 
## example usage:
## M <- matrix(rnorm(36),6,6)
## Mf <- makeCacheMatrix(M)
## cachesolve(Mf)

## the first function provides the getter and the setter for caching the matrix data and its inverse

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


## the second function retrieves the matrix inverse from cache when available, otherwise calculates and caches the matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
