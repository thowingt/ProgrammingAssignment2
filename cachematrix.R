# makeCacheMatrix
# Creates a special "matrix" object that can cache its inverse.
# To start, set inv (the inverse of x) to NULL.
# If setting the value of the matrix, set x to this value (y), and set inv to NULL.
# If getting the value of the matrix, return x.
# If setting the inverse, make inv equal to the inverse of x.
# If retrieving the inverse, return inv.
# Return a vector of functions that set or get the value of x, or set or get the inverse of x.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# cacheSolve
# Computes the inverse of the special "matrix" returned
# by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.
# Set inv to the value from makeCacheMatrix.  It may ne NULL.
# If the value of inv is not NULL, use it and return it.
# If the value of the inverse is NULL, find the inverse, set it, and return it.

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
