## These functions provide the capability to create and cache the inverse
## of a matrix for subsequent retrieval. If the inverse is requested, the
## cache is first checked. If the inverse is found, it is returned. If it 
## is not found, it is computed, then stored for later use.

## Example use:
##      myMatrix <- makeCacheMatrix(matrix(c(4,3,3,2), nrow = 2, ncol = 2))
##      myInverse <- cacheSolve(myMatrix)  #computed inverse
##      myInverse <- cacheSolve(myMatrix)  #inverse retrieved from cache


## This function defines a wrapper around a matrix that allows the 
## values for the matrix and its inverse to be set and retrieved.

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


## This function returns the inverse of a matrix created
## with makeCacheMatrix. It first checks to see if the
## inverse has been previously stored; if so, it returns
## the stored value; if not, it computes it and stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        message("computing inverse")
        library(MASS)
        m <- ginv(data, ...)
        x$setinverse(m)
        m
}
