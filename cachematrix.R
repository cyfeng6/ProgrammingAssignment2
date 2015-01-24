## a pair of functions that cache the inverse of a matrix
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set.inverse  <- function(inverse) m <<- inverse
  get.inverse <- function() m
  ## function makeCasheMatrix will return as a Matrix
  ## There are 4 argumnets in it.
  Matrix(set = set, get = get,
       set.inverse  = set.inverse  ,
       get.inverse = get.inverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ##x$get.inverse can call x and check if
  ## x has been caculated or not
  
  m <- x$get.inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##if x is not null, meant it's been caculated.
  ## Will get the value
  data <- x$get()
  m <- inverse(data, ...)
  x$ set.inverse(m)
  m
  
}
