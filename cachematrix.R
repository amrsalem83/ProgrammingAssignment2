## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	inv = NULL
      set = function(y) {
              x <<- y
              inv <<- NULL
      }
      get = function() x
      setInverse = function(inverse) inv <<- inverse 
      getInverse = function() inv
      list(set=set, get=get, setInverse=setInverse, getInverse=getInverse )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
	inverse = x$getInverse()
      if (!is.null(inverse)){
		# check if its available in the cache
		# get it from the cache and skips the calculation. 
            return(inverse)
      }
       
      # if not available in the cache, then do the calculation
      inverse = solve(x$get())

      # set the value of the inverse to the cache using setinverse
      x$setInverse(inverse)
      return(inverse)
}
