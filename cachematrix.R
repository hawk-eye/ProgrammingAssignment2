## makeCacheMatrix: This function creates a special "matrix" object that can
##                  cache its inverse.
## cacheSolve:      This function computes the inverse of the special "matrix"
##                  returned by makeCacheMatrix above. If the inverse has
##                  already been calculated (and the matrix has not changed),
##                  then the cachesolve should retrieve the inverse from the
##                  cache.

## makeCacheMatrix - Returns a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(thisInverse) inverse <<- thisInverse
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the mean of the data and sets the
## value of the mean in the cache via the setmean function.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
