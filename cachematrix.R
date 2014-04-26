## Script allowing to cache the inverse of an invertible matrix.
## It is very inspired from the exercise example.

## makeCacheMatrix creates a special matrix which can cache its inverse.
## We assume the matrix is invertible.
## The special matrix contains 4 functions:
## * get: get the value of the matrix
## * set: set the value of the matrix
## * setinverse: set the value of the inverse
## * getinverse: get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse_cache <- NULL
        set <- function(y) {
                x <<- y
                inverse_cache <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse_cache <<- inverse
        getinverse <- function() inverse_cache
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created
## with the makeCacheMatrix function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the inverse of the data and sets the
## value of the inverse in the cache via the setinverse function.
## This function description is also very inspired from the exercise's example.
cacheSolve <- function(x, ...) {
        inverse_cache <- x$getinverse()
        if(!is.null(inverse_cache)) {
                message("getting cached data")
                return(inverse_cache)
        }
        data <- x$get()
        inverse_cache <- solve(data, ...)
        x$setinverse(inverse_cache)
        inverse_cache
}
