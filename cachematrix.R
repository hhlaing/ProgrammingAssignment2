## Calculating inverse of a matrix (especially larg matrix) is an expensive
## operation. Therefore, having a special matrix wrapper that allows caching
## of its inverse is useful. A special function is also needed that can take
## the special matrix and return its inverse where cache inverse is used where
## possible.  The following two functions provide that capability

## Function that takes in a matrix and returns an object that can cache the
## matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverseM <- NULL
    set <- function(y) {
        x <<- y
        inverseM <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) inverseM <<- inverse
    getInverse <- function() inverseM
    list(set= set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Function that takes the special matrix and returns the inverse matrix.
## If inverse matrix is found in cache, the cache value is used. If not,
## inverse is calculated and cache it in the special matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverseM <- x$getInverse()
    if (!is.null(inverseM)) {
        message("getting cached inverse matrix")
        return(inverseM)
    }
    data <-x$get()
    inverseM <- solve(data, ...)
    x$setInverse(inverseM)
    inverseM
}
