# Caching the Inverse of a Matrix
# by LZ 0914/2020
#

# This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    set <- function (y) {
        x <<- y
        invM <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invM <<- inverse
    getinverse <- function() invM
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function computes the inverse of the special matrix returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invM <- x$getinverse()
    if (!is.null(invM)) {
        message("getting cached data")
        return(invM)
    }
    data <- x$get()
    invM <- solve(data)
    x$setinverse(invM)
    invM
}