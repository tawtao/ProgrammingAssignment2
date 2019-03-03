## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix(m) is a function take a matrix and
## return a list of function that maintain the
## cache of matrix and the cache inverse of the matrix. 
## m by default is an empty matrix.
## The functions are:
## * cm <- makeCacheMatrix(m) :create a cache of matrix m, and 
##                             initalize the cache of inverse matrix m to NULL
## * cm$set(m)      :assign the cache matrix to matrix m, and
##                   reinitalize the cache of inverse matrix m
## * cm$get()       :access the cache matrix
## * cm$setsolve(s) :assign the cache of inverse matrix m to s
## * cm$getsolve()  :access the cache of inverse matrix m
##
makeCacheMatrix <- function(x = matrix()) {
  sol <- NULL
  set <- function(y) {
    x <<- y
    sol <<- NULL
  }
  get <- function() x
  setsolve <- function(s) sol <<- s
  getsolve <- function() sol
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Write a short comment describing this function
## cacheSolve(x, ...) function return the inverse of 
## the matrix stored in x.
##
## The cacheSolve will compute the inverse and then
## store in the cache matrix x for future access.
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  sol <- x$getsolve()
  if( !is.null(sol) ) {
    message("getting cached data")
    return(sol)
  }
  data <- x$get()
  sol <- solve(data, ...)
  x$setsolve(sol)
  sol
}

## The following code is served as examples and tests of 
## The makeCacheMatrix and cacheSolve functions.
## Uncomment the code to run it.
#
## create a matrix ma
# ma = matrix(c(5, 1, 0,
#               3,-1, 2,
#               4, 0,-1), nrow=3, byrow=TRUE)
#
## create a matrix mb
# mb = matrix(c(4, 2, 2,
#               2, 3, 1,
#               2, 1, 3), nrow=3, byrow=TRUE)
#
## create a cache matrix ca from a matrix ma
# ca = makeCacheMatrix(ma)
#
## access the matrix ma
# cm$get()
#
## Now calculate inverse and cache
# cacheSolve(cm)
#
## Now, try to calculate inverse again
# cacheSolve(cm)
#
## Let set the cm to a new matrix mb
# cm$set(mb)
#
## Try cacheSolve cm again,
# cacheSolve(cm)
#
## And again to get the cache data
# cacheSolve(cm)
