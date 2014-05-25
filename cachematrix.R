## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix is a function that takes a matrix as input
## It allows to set and get the value of the matrix in the cache
## It also allows to get and set the inverse value in the cache

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


## Write a short comment describing this function

## cacheSolve is a function that takes as input an object
## In this assignment the object is a matrix
## The output is the inverse of the input matrix
## Note that if the inverse matrix value is already in the cache
## then this function allows to directly retrieve the value and it prints
## a notification message to indicate that cache value is retrieved
## Else if the value is not in the cache (not previouslt computed)
## then the inverse is computed and returned

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if ( ! is.null(m)) {
    print("getting cached data")
    return(m)
  }
  m <- solve(x$get())
  x$setinverse(m)
  m
}
