## Below are two functions which is used to create a
## special object which stores a matrix and cache's
## it's inverse

## makeCacheMatrix function creates a special "vector", 
## which is really a list containing a function to
## set() - set the value of the matrix
## get() - get the value of the matrix
## setinv() - set the value of the inverse
## getinv() - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv=getinv)
}

## cacheSolve function calculates the inverse of the 
## special "vector" created with the above function. 
## However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the matrix and sets the 
## value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting the cached value of the inverse..")
    return(inv)
  }
  message("No cached value available. Finding the inverse..")
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
