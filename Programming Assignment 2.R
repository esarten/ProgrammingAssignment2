## These functions serve to fulfill the requirements of Programming Assignment 2 
## and specifically fulfill the following functions

## this function creates an object in the form of a matrix and then uses the
## solve function to then cache its invers

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## this function will either return the inverse of a matrix if it has already 
## been computed and stored or it will return the inverse of the matrix created 
## above

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
}
