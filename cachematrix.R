## Put comments here that give an overall description of what your
## functions do

## stores symmetrical matrix in cache. must first define matrix and use that
## object as x
## example
## matrix1 <- matrix(1:4, 2, 2)
## matrixcache <- makeCacheMatrix(matrix1)

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


## solves matrix defined above if not already done

cacheSolve <- function(x, ...) {
    #returns matrix that is inverse of x
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