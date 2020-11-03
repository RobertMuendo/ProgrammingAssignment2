## MAKE CACHE MATRIX FUNCTION
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
          x <<- y
          inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(matrixinverse) inverse <<- matrixinverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## CACHE SOLVE FUNCTION
cacheSolve <- function(x, ...) {
  inverse <= x$getmatrixinverse()
  if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setmatrixinverse(inverse)
  inverse
}
