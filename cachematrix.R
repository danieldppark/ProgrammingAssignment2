## Pair of functions that cache and compute the inverse of a matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(pmtx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    pmtx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(pmtx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the inverse of the special matrix returned by the "makeCacheMatrix" function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- pmtx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- pmtx$get()
  invserse <- solve(data, ...)
  pmtx$setinv(inverse)
  return(inverse)
}
