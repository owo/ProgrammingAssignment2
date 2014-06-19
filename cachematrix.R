## This file provides two functions that implement matrix inverse caching.
## 'makeCacheMatrix' creates a Cache Matrix, a special matrix that is capable
## of caching its inverse. 'cacheSolve' takes a Cache Matrix and either returns
## the cached inverse if already computed, or computes the inverse and caches
## it the Cache Matrix for later use.

## @author: Ossama W. Obeid


## 'x' - a square numeric or complex matrix
## Returns a Cache Matrix, a wrapper around matrix 'x' that caches its
## inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## 'x' - a Cache Matrix returned by calling 'makeCacheMatrix'
## Returns the inverse of 'x'
cacheSolve <- function(x) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
