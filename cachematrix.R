## Functions to enable caching of a solved matrix
#
#  Example Usage:
#    my_matrix <- matrix(c(1, 3, 2, 4), 2, 2))
#    my_cached_matrix <- makeCacheMatrix(my_matrix)
#    cacheSolve(my_cached_matrix)  # calculation occurs here
#    foo <- do_other_things()
#    cacheSolve(my_cached_matrix)  # will return cached data FTW!


## makeCacheMatrix
#
#  Make a "cached" matrix for used with cachedSolve
#
#  Arguments
#    x  initial value of matrix to use (defaults to emopty matrix)
#
#  Returns
#    List of functions that will be used by cachedSolve

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve
#
#  Solve a matrix or return a cached inverse if already solved
#  Uses underlying solve(...) function on the "cached" matrix
#
#  Arguments
#    "cached" matrix - must be a value returned from makeCacheMatrix
#
#  Returns
#    inverse of matrix which may be a cached value

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
    
}
