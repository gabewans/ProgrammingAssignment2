## Below are two functions that are used to create a special object that
## stores a matrix and calculates its inverse.

## This function will contain four functions (1) sets the value of the matrix
## (2) returns the value of the matrix (3) sets the value of the inverse of the matrix
## and (4) returns the value of the inverse of the matrix

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


## This function will check to see if the inverse of the matrix is already cached
## and if so will return the cached value; otherwise it will compute the inverse
## and cache the result.

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
