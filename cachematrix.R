## Put comments here that give an overall description of what your
## functions do

## Function creates a special opject that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function computes the inverse of the special object returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("Getting cached data.")
                return(inv)
                }
        matrix <- x$get()
        inv <- solve(matrix,...)
        x$setInverse(inv)
        inv
}
