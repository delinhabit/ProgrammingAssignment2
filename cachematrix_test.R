source('cachematrix.R')

test_makeCacheMatrixReturnsListOfFunctions <- function() {
    l <- makeCacheMatrix()
    checkEquals(class(l), "list")
    checkEquals(names(l), c("set", "get", "setInverse", "getInverse"))
}

test_get <- function() {
    x <- matrix(rnorm(9), 3, 3)
    l <- makeCacheMatrix(x)

    checkEquals(l$get(), x)
}

test_set <- function() {
    l <- makeCacheMatrix()
    x <- matrix(rnorm(9), 3, 3)
    l$set(x)

    checkEquals(l$get(), x)
}

test_getInverse <- function() {
    l <- makeCacheMatrix()

    checkTrue(is.null(l$getInverse()))
}

test_setInverse <- function() {
    x <- matrix(rnorm(9), 3, 3)
    invx <- solve(x)

    l <- makeCacheMatrix(x)
    l$setInverse(invx)

    checkEquals(l$getInverse(), invx)
}

test_setDeletesInverse <- function() {
    x <- matrix(rnorm(9), 3, 3)
    invx <- solve(x)

    l <- makeCacheMatrix(x)
    l$setInverse(invx)
    l$set(matrix(rnorm(9), 3, 3))

    checkTrue(is.null(l$getInverse()))
}

test_cacheSolveReturnInverse <- function() {
    x <- matrix(rnorm(25), 5, 5)
    l <- makeCacheMatrix(x)
    inv <- cacheSolve(l)

    checkEquals(inv, solve(x))
}

test_cacheSolveReturnInverseFromCacheSecondTime <- function() {
    x <- matrix(rnorm(25), 5, 5)
    l <- makeCacheMatrix(x)
    inv1 <- cacheSolve(l)

    tryCatch({
        inv2 <- cacheSolve(l)
        checkEquals(inv2, inv1)
    }, message = function(m) {
        checkEquals(
            m$message,
            "Getting cached inverse\n")
    })
}