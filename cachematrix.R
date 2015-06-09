## This is the functions for open course "R Programming" assignment 2: Caching the Inverse of a Matrix
## Created by GEOX on 3/17/2015

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  myInverse <- NULL                   # initialize the inverse property
  set <- function(y) {
    x <<- y
    myInverse <<- NULL                # if the matrix is changed, reset the inverse to NULL
  }
  get <- function() x                 # get function for getting matrix to be computed
  setInverse <- function(inverse) myInverse <<- inverse     # store the inverse
  getInverse <- function() myInverse  # get the inverse
  list(set = set, get = get,          # create the "cache matrix" object: a list of 4 functions
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  myInverse <- x$getInverse()      # assign the inverse stored in the special "matrix" object x
  if(!is.null(myInverse)) {        # if the inverse is stored, get the cached data and return
    message("getting cached data")
    return(myInverse)
  }
  matrix <- x$get()                # if the inverse is not computed, iget the matrix data
  myInverse <- solve(matrix, ...)  # calculate the inverse
  x$setInverse(myInverse)          # store the inverse matrix
  myInverse                        # return the inverse
}
