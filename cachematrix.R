## Put comments here that give an overall description of what your
## functions do

## This file contains two functions:
## 1. makeCacheMatrix: creates a special "matrix" object that can cache its inverse
## 2. cacheSolve: computes the inverse of the matrix returned by makeCacheMatrix,
##    or returns the cached inverse if it has already been computed.

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  get <- function() {
    x
  }

  setinverse <- function(inverse) {
    inv <<- inverse
  }

  getinverse <- function() {
    inv
  }

  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## This function returns the inverse of the matrix stored in the special object.
## If the inverse has already been computed, it retrieves it from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()

  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
