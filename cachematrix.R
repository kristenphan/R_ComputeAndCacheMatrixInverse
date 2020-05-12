## the two functions below leverages R's lexical scoping to optimize the computation of inverse matrix
## 'makeCacheMatrix' function creates a special object that holds a matrix and its inverse for caching purpose
## 'cacheSolve' function attempts to retrieve the inverse matrix from the passed in object x creately previoused using 'makeCacheMatrix'
## if no cache is found, this function computes the inverse matrix and saves it in object x



## this function creates an object that stores matrix x and its inverse
## the object supports 4 functions:
## x$set(): replace matrix x with a new matrix and reset the inverse matrix
## x$get(): return matrix x
## x$setinv(): set the inverse to the passed in argument 'inverse'
## x$getinv(): return the inverse matrix
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


## this function attempts to cache and return the inverse of the passed in object x (object x must be previously created using function 'makeCacheMatrix')
## if no cache is found, the function computes and returns the inverse matrix of matrix x and saves the cache in object x 
## (object x contains matrix x and the inverse matrix)
cacheSolve <- function(x, ...) {
  ## attempt to retrieve the inverse of matrix x from cache
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if no cache is found, compute the inverse and save it in cache
  matrix_dat <- x$get()
  inv <- solve(matrix_dat) 
  x$setinv(inv)
  inv
}
