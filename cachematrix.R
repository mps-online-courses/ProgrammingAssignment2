## Coursera R-Programming Peer Assignment
## Caching the inverse of a matrix

## Example:
## source("makeCacheMatrix.R")
## m <- makeCacheMatrix(matrix(1:6, 2, 2))
## cacheSolve(m)
## prints "solving matrix"
## cacheSolve(m)
## prints "getting cached data"

## `makeCacheMatrix` creates a special (wrapped) matrix that can store its inverse
## so it only has to be computed once in order to save computation time.
## x must be a square matrix for it to be invertible.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL # this will be our cached value
  # set allows to change the matrixes value
  set <- function(y) {
    x <<- y
    m <<- NULL # invalidate cache when setting to a different value
  }
  get <- function() x
  set_inverse <- function(new_inverse) inverse <<- new_inverse
  get_inverse <- function() inverse
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

## `cacheSolve` is a specialized version of `solve` that uses our wrapped matrix
## to cache the inverse (computed by solve).
## Input must be a square matrix created by `makeCacheMatrix`.
cacheSolve <- function(x, ...) {
  # First, try to read the value if it was previously computed
  inverted <- x$get_inverse()
  if (is.null(inverted)) {
    # Not yet computed, so let's compute it now
    message("solving matrix")
    value <- x$get() # get the actual, underlying R matrix
    computed_inverted <- solve(value, ...)
    x$set_inverse(computed_inverted) # fill the cache
    computed_inverted
  } else {
    # it was already computed, so no work necessary
    message("getting cached data")
    inverted
  }
}
