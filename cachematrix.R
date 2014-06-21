## The following functions provide the ability to create a matrix that can cache its
## own inverse

## The following function creates a special matrix that provide operations to set and 
## get the matrix and its inverse. The inverse is computed lazily when required using 
## the cacheSolve function. If the matrix is not a square matrix, it will provide
## a warning and set the matrix to be an empty matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    if (nrow(x) != ncol(x)) {
        warning("x is not a square matrix. Will be set to an empty matrix")
        x <- matrix()
    }
    
    set <- function(y) {
        if (nrow(y) == ncol(y)) {
            x <<- y
            inv <<- NULL
        } else {
            warning("x is not a square matrix. Will be set to an empty matrix")
            x <<- matrix()
            inv <<- NULL            
        }
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Use cacheSolve to evaluate the inverse of a matrix created using makeCacheMatrix
## function. This function will check if the inverse has already been calculated and 
## if so, return the pre-calculated inverse. Otherwise, it will calculate the inverse,
## cache it in makeCacheMatrix and return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("Getting cached inverse")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setInverse(inv)
    inv
}
