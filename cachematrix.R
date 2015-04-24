## Two functions designed to work together for the purpose of caching 
## inverse of a matrix.

## This function form environment to store (cache) matrix and it`s inverse
## and constructs the list of functions for set/get matrix and it`s inverse  
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    setMatrix <- function(y) {
        x <<- y
        i <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inv) i <<- inv
    getInverse <- function() i
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function calculate and return inverse of matrix and save it so 
## subsequent calls skip calculation step.
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        return(i)
    }
    data <- x$getMatrix()
    i <- solve(data, ...)
    x$setInverse(i)
    ## Return a matrix that is the inverse of 'x'
    i
}
