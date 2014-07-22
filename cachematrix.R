## The following functions cache the inverse of a matrix rather than compute it
## repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # inv will store the cached inverse matrix.
    inv <- NULL

    # Set the matrix to the new value and invalidate any cached inverse.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # Get the value of the matrix.
    get <- function() x

    # Set (cache) the inverse of the matrix.
    setinverse <- function(inverse) inv <<- inverse

    # Get the cached inverse of the matrix.
    getinverse <- function() inv

    # Return the list of the getter and setter functions.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    # Get the cached inverse if available.
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }

    # If the inverse is not cached, then calculate it and cache it.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)

    # Return the calculated inverse.
    inv
}
