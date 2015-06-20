# The following is a pair of functions that CACHE and COMPUTE the inverse of a matrix.

# This function creates a special "matrix" object that can CACHE its inverse

makeCacheMatrix <- function(mtx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mtx <<- x;
    inverse <<- NULL;
  }
  get <- function() mtx;
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

# This function COMPUTES the inverse of the special "matrix" returned by the function `makeCacheMatrix` above.
# If the inverse has already been calculated (and the matrix has not changed), then the function `cacheSolve`
# should retrieve the inverse from the cache

cacheSolve <- function(mtx, ...) {
  inverse <- mtx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mtx$get()
  inverse <- solve(data, ...)
  mtx$setinv(inverse)
  return(inverse)
}
