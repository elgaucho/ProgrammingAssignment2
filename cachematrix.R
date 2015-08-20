
## This function creates a matrix (presumed to be invertible) and includes
## other functions to set the matrix values, retrieve them (get), as well
## as set and retrieve the values of the inverse matrix

makeCacheMatrix <- function(matrix = matrix()) {
    matrix_inverse <- NULL
    set <- function(set_matrix) {
        matrix <<- set_matrix
        matrix_inverse <<- NULL
    }
    get <- function() matrix
    setinverse <- function(inverse) matrix_inverse <<- inverse
    getinverse <- function() matrix_inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function retrieves the inverted matrix, tests if it exists, and,
## if not, computes the inverted matrix and sets the values of the inverted
## matrix. If the solution already exists, this function retrieves it from
## the cache.

cacheSolve <- function(matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    matrix_inverse <- matrix$getinverse()
    if(!is.null(matrix_inverse)) {
        message("getting cached data")
        return(matrix_inverse)
    }
    data <- matrix$get()
    matrix <- solve(data, ...)
    matrix$setinverse(matrix_inverse)
    matrix_inverse
}
