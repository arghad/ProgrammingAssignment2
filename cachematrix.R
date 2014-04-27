## Put comments here that give an overall description of what your
## functions do

## This function will make a cacheMatrix which is a special matrix that caches its inverse
## please be sure that the matrix x is a squared matrix

makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    set <- function(y) {
        x <<- y
        invMat <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) invMat <<- solve
    getInverse <- function() invMat
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function calculates the inverse of a matrix, if the inverse is already calculated then it will be returned directly without any calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'    
    invMat <- x$getInverse()
    if(!is.null(invMat)) {
        message("getting cached data")
        return(invMat)
    }
    data <- x$get()
    invMat <- solve(data, ...)
    x$setInverse(invMat)
    invMat
}
