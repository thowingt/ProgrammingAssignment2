
## Put comments here that give an overall description of what your
## functions do

##  Creates a special "matrix" object that can cache 
##  its inverse

makeCacheMatrix <- function(x = matrix()) {
  #set inv (the inverse of x) to NULL
  inv <- NULL
  # if setting the value, set x to this value (y), and set inv to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # if getting the value of the matrix, return x
  get <- function() x
  # if setting the inverse, make inv equal to the inverse of x
  setinv <- function(solve) inv <<- solve
  # if retrieving the inverse, return inv
  getinv <- function() inv
  # return a vector of functions that set or get the value of x, or set or get the inverse of x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # set inv to the value that has already been calulated
  inv <- x$getinv()
  # if the value of inv is not NULL, use it and return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if there is not already an inverse, find the inverse, set it, and return it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
