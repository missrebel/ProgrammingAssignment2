## Put comments here that give an overall description of what your
## functions do
## The goal of my 2 functions is to increase speed while calculating the inverse of a matrix. If the inverse of a particular matrix has already been calculated, 
## the result of the inversion is put into the cache and is not recalculated again while calling cacheSolve.

## Write a short comment describing this function
## allows to set or get the matrix x or set/get the inverse of the matrix X
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
## Write a short comment describing this function
## Calculate the inverse of the matrix x if not already existing in the cache. Set the inv if bot already done.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
