## functions that help to cache the computation of matrix inversions

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## 'x' is the original matrix.
    ## Return a list of functions:
    ##  set - updates the matrix
    ##  get - returns the matrix
    ##  setInverse - updates the matrix's inverse
    ##  getInverse - returns the matrix's inverse
    inv <- NULL
    list(
         set = function(data) {
             x <<- data
             inv <<- NULL
         },
         get = function() {
             x
         },
         setInverse = function(inverse) {
             inv <<- inverse
         },
         getInverse = function() {
             inv
         }
    )
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
    ## x is a special matrix created by makeCacheMatrix.
    ## Returns the inverse of x, caching it when solved for first time.
    inv <- x$getInverse()
    if (is.null(inv)) {
        data <- x$get()
        message('solving...')
        inv <- solve(data, ...)
        x$setInverse(inv)
    }
    inv
}

