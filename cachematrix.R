## This is the functions for open course "R Programming" assignment 2: Caching the Inverse of a Matrix
## Created by GEOX on 3/17/2015

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  myInverse <- NULL     # initialize the inverse property
  set <- function(y) {
    x <<- y
    myInverse <<- NULL    # if the matrix is changed, reset the inverse to NULL
  }
  get <- function() x
  setInverse <- function(inverse) myInverse <<- inverse
  getInverse <- function() myInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  myInverse <- x$getInverse()
  if(!is.null(myInverse)) {
    message("getting cached data")
    return(myInverse)
  }
  matrix <- x$get()
  myInverse <- solve(matrix, ...)
  x$setInverse(myInverse)
  myInverse
}
