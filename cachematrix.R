## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## x is a square invertible matrix
## The function returns: a list containing functions to  set the matrix, get the matrix, set the inverse,get the inverse
## this list is the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <-
    function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## x is output of makeCacheMatrix()
## The function returns: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinv(inv)
  inv
}
