
## This function creates a matrix (presumed to be invertible) and includes
## other functions to set the matrix values, retrieve them (get), as well
## as set and retrieve the values of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function retrieves the inverted matrix, tests if it exists, and,
## if not, computes the inverted matrix and sets the values of the inverted
## matrix. If the solution already exists, this function retrieves it from
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
