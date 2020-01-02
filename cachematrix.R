## Put comments here that give an overall description of what your
## functions do
## two functions that 1)create a cached matrix 2)finds inverse of the matrix

## Write a short comment describing this function
## creates special matrix that caches inverse of input

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function
## computes inverse of special matrix if inverse has not been calculated
## if it has been, inverse of matrix is returned from cache

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
