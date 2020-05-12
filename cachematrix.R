## Put comments here that give an overall description of what your
## functions do

## this function creates an object that stores matrix x and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## this function attempts to cache and return the inverse of the passed in object x (object x must be created using function 'makeCacheMatrix')
## if no cache is found, the function computes and returns the inverse matrix of matrix x
## and saves the cache in object x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_dat <- x$get()
  inv <- solve(matrix_dat)
  x$setinv(inv)
  inv
}
