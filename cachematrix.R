## The goal of these functions is to reduce the redundant calculation of the inverse matrix
## The functions work together in order to store and refer to past inverse matrices calculated.

## makeCacheMatrix receives a matrix input and calculates and stores the inverse
## of the input matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- matrix()
    set <- function(y) {
        x <<- y
        m <<- matrix()
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function accepts the matrix input x and the object makeCacheMatrix where the information on matrix x is stored.
## If the inverseve matrix has not yet been calculated or needs to be recalculated it is done in this function.

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

