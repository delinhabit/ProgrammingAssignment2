## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    list(
        set = function(y) {
            x <<- y
            inv <<- NULL
        },
        get = function() x,
        setInverse = function(inverse) inv <<- inverse,
        getInverse = function() inv
    )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("Getting cached inverse")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
