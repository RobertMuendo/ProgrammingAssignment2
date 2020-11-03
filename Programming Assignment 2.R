## MAKECACHEMATRIX FUNCTION
### This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) { #### Sets the value of the matrix.
          x <<- y
          inverse <<- NULL
  }
  get <- function() x #### Gets the value of the matrix.
  setinverse <- function(matrixinverse) inverse <<- matrixinverse #### Sets the value of the inverse.
  getinverse <- function() inverse #### Gets the value of the inverse.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## CACHESOLVE FUNCTION
### This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse <= x$getinverse() #### Returns a matrix that is the inverse of 'x'.
  if(!is.null(inverse)) { #### Identifies whether inverse has been computed.
          message("getting cached data")
          return(inverse) 
  }
  data <- x$get() #### This utilizes tthe setinverse function to compute inverse if it had not yet been computed.
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
