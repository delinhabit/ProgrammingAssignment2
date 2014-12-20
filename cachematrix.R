######################################################################
## Copyright (C) 2014, Ion Scerbatiuc <delinhabit@gmail.com>
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA
#####################################################################

#####################################################################
## This module provides two utility functions for computing and using
## the cached version of a matrix.
#####################################################################


makeCacheMatrix <- function(x = matrix()) {
    # Creates a list of functions that encapsulates a matrix and provides means
    # for retrieving and storing it's inverse.
    #
    # Store the value of the inverse by calling the `setInverse` function.
    #
    # The value of the inverse is cached for future calls of `getInverse`,
    # unless the value of the matrix is changed using the `set` function. You
    # can retrieve the raw matrix by calling the `get` function.
    #
    # Args:
    #   x: the raw matrix to encapsulate
    #
    # Returns:
    #   A list of functions `set`, `get`, `setInverse` and `getInverse` as
    #   described above
    #
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


cacheSolve <- function(x, ...) {
    # Computes the inverse of the provided "cache matrix" and caches it for
    # future uses.
    #
    # If the inverse of the matrix, was already computed, retrieve it from
    # the cache.
    #
    # Args:
    #   x: the "cache matrix" list representing the matrix to be solved
    #
    # Returns:
    #   The inverse of the matrix
    #
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("Getting cached inverse")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
