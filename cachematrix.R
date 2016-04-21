## These functions return the inverse of a given matrix.

## makeCacheMatrix applies a list of functions to the supplied matrix
## that set and get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setmatrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse) 
}


## cacheSolve solves the supplied matrix (of the type returned by
## makeCacheMatrix) and caches the inverse for later use. If the matrix has
## already been solved, cacheSolve returns the inverse.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$getmatrix()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
