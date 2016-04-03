## Library of functions that amortize the cost of inverting matrices
## by caching the result from `solve()`.

## Wrapper function returning a list of accessor functions:
##  * `get()`:
##      Returns the original matrix.
##  * `set(new_matrix)`:
##      Sets original matrix to new matrix and
##      invalidates the cached inverse.
##  * `get_inverse():`
##      Returns the cached inverse matrix.
##  * `set_inverse(new_inv)`:
##      Sets the cached inverse matrix.
##
## Intended to be used with `cacheSolve`.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() {
        x
    }
    set <- function(new_matrix) {
        x <<- new_matrix
        inv <<- NULL
    }
    get_inverse <- function() {
        inv
    }
    set_inverse <- function(new_inv) {
        inv <<- new_inv
    }
    list(get = get,
         set = set,
         get_inverse = get_inverse,
         set_inverse = set_inverse)
}


## Returns the inverse of a `makeCacheMatrix`-compatible matrix.
## Uses the cached inverse if available; otherwise, calculates the
## inverse and caches it for subsequent calls.
cacheSolve <- function(x, ...) {
    cached_inverse <- x$get_inverse()
    if (!is.null(cached_inverse)) {
        message("getting cached inverse")
        return(cached_inverse)
    }
    the_matrix <- x$get()
    calculated_inverse <- solve(the_matrix)
    x$set_inverse(calculated_inverse)
    calculated_inverse
}
