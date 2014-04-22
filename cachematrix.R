## cachematrix.R is comprised of two functions which are used to
## create a "special matrix" and calculate the inverse of the matrix.
## These functions assume that the matrix provided is invertible.

## makeCacheMatrix fucntion creates the "special matrix."  It takes in a matrix and
## returns a "special matrix."  The "special matrix" is a list of functions 
## used to set the matrix, get the matrix, set the inverse, and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseMatrix <- inverse
    getInverse <- function() inverseMatrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function takes in a "special matrix," checks to see if the inverse
## exists, and either returns the cached inverse matrix or uses solve() to create
## the inverse, caches it, and returns the inverse matrix.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverseMatrix <- solve(data, ...)
    x$setInverse(inverseMatrix)
    inverseMatrix
}
