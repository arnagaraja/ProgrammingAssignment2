## This set of functions will compute the inverse of a matrix and return it 
## or will return a cached version of the inverse, if one has already been computed.

## This function returns a list of functions that do the following tasks:
## set(): sets the matrix
## get(): gets the matrix
## setInverse(): stores the inverse of the matrix
## getInverse(): retrieves the inverse of the matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
      invx <- NULL
      set <- function(y) {
            x <<- y
            invx <<- NULL
      }
      get <- function() x
      setInverse <- function(solved) invx <<- solved
      getInverse <- function() invx
      list(set = set, get = get, setInverse = setInverse, 
                                    getInverse = getInverse)
}


## This function will either return the cached inverse of a matrix, or if the 
## inverse hasn't been calculated yet, it will do the calculation and cache it
## for later retrieval

cacheSolve <- function(x, ...) {
      invx <- x$getInverse()
      if(!is.null(invx)) {
            message("getting cached data")
            return(invx)
      }
      data <- x$get()
      invx <- solve(data, ...)
      x$setInverse(invx)
      invx
}
