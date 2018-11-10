## Put comments here that give an overall description of what your
## functions do
# The ensuing program consists of two functions namely, makeCacheMatrix and cacheSolve.
# The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
# The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.


## Write a short comment describing this function
# makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#cacheSolve


cacheSolve <- function(x, ...) {
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
