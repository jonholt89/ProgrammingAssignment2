## makeCacheMatrix creates a special matrix object that can cache its inverse 
## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

## Here we will create the inverse matrix object

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}
# Now we have created a list of four functions to get and set
# the matrix to be used in the subsequent function.

## Here we will compute the inverse matrix of matrix x

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
          # If the inverse has already been calculated, retrieve the cache
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        # If the inverse has yet to be calculated, then calculate it now
}
