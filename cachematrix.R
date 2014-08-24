## These functions will cache computations for finding the inverse
## of a matrix. The inverse of the matrix multiplied by the
## original matrix should provide the identity matrix.

## makeCacheMatrix creates a matrix-like object that will cache
## its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
        
}


## Using the solve() function, cacheSolve computes the
## inverse of the object created in the makeCacheMatrix
## function. If the inverse already exists, then cacheSolve
## retrieves the inverse from the cache, not recalculating
## and thus saving time.

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
