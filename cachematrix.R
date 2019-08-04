## Put comments here that give an overall description of what your
## functions do
## Chen Xing 2019-Aug-05

## The first function, makeVector creates a special "vector", 
## which is really a list containing a function to
#  1) set the value of the vector
#  2) get the value of the vector
#  3) set the value of the mean
#  4) get the value of the mean
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
## The following function calculates the mean of the special "vector" 
## created with the above function
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
## ==== Above is an example. ====


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Initializing as NULL  
  m <- NULL
  
  ## Set the value of matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Get the value of matrix
  get <- function() x
  
  ## Set the inv
  setinv <- function(inv) m <<- inv
  
  ## Get the inv
  getinv <- function() m
  
  ## Return the list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  
  ## Check to see if we have created the inverse already
  if(!is.null(m)) {
    message("getting inversed data") ## Did created before, and return the m
    return(m)
  }
  
  ## If did not get the matrix, we calculate the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}