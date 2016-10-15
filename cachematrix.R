## Caching the Inverse of a square invertable Matrix


makeCacheMatrix <- function(x = matrix(0)) {

## Creates a special "matrix" object that can cache its inverse.
## The input is a square invertable matrix
      
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get= get,
           setinv = setinv,
           getinv = getinv)
}   




cacheSolve <- function(x, ...) {

##Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
      
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      if (ncol(data)!=nrow(data)) {
            stop('the input matrix should be square')
      }
      else {
            inv <- solve(data, ...)
            x$setinv(inv)
            inv
      }
}
