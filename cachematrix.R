## These  function are used to cache a matrix in which an inverse operation could
## be called on it.

## makeCacheMatrix creates a vector which is a list containg set and get  functions for
## the matrix and the inverse of that matrix.

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


## cacheSolve will calculate the inverse of the matrix that was created with
## makeCacheMatrix. However, first it will check to see if the inverse has already
## been created, and if it has, it will use the cached version of the inverse, to save
## computing time and resources.

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
