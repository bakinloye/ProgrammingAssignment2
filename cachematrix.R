## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    j <- NULL
    set <- function(y) {
        x <<- y
        j <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) j <<- inverse
    getinverse <- function() j
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    j <- x$getinverse()
    if (!is.null(j)) {
        message("getting cached data")
        return(j)
    }
    data <- x$get()
    j <- solve(data, ...)
    x$setinverse(j)
    j
}
