## These 2 functions in combination provide the ability to compute and cache the inverse of a matrix. They
## will be useful in cases where the inverse of the same matrix needs to be accessed many times. Only the
## first time the inverse is asked for, will it be calculated. Subsequent calls will result in the cached value being 
## returned.


## This function returns a list of getter and setter functions for both the matrix x itself - passed as a parameter - 
## as well as the inverse of the matrix. It needs to be called before the cacheSolve function is called.
 
makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## cacheSolve takes a list x. The list needs to be a list that was returned from a call to the 
## above makeCacheMatrix function. cacheSolve returns a matrix that is the inverse of the matrix that was passed to 
## the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
