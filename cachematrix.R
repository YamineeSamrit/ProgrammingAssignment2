## These functions implement caching of a matrix with its inverse.
## cache <- makeCacheMatrix(matrix) creates an initialized cache.
## cache$get() returns the stored matrix.
## cache$set(matrix) changes the stored matrix.
## cacheSolve(cache) solves (or only accesses) the inverse.


## Creates a cache object initialized with the passed matrix.

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y){
              x <<- y
              inv <<- NULL
            }
            get <- function() {x}
            setInverse <- function(inverse) {inv <<- inverse}
            getInverse <- function() {inv}
            list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Solves, stores, and returns the inverse of the cached matrix,
## or only returns it if it already exists.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
}
