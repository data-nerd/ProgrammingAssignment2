## The following functions create and populate "cache matrix" objects
## that can significantly improve performance of operations that require
## solving matrix inverses multiple times.
##
## Sample usage:
##    M <- matrix(rnorm(16), 4, 4)
##    c <- makeCacheMatrix(M)
##    cacheSolve(c)
## Subsequent calls to cacheSolve(c) will return the cached inverse
##
## See the comments on each function for more details.

## makeCacheMatrix creates a special "matrix" object that can cache
## the inverse of a matrix alongside the original. The returned value
## is actually a list with functions to access the original matrix and
## its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
	cachedInverse <- NULL
	set <- function(y) {
		x <<- y
		cachedInverse <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) cachedInverse <<- inverse
	getInverse <- function() cachedInverse
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## cacheSolve returns the inverse of a matrix, utilizing the special
## "matrix" created by makeCacheMatrix. If the inverse has not been
## calculated previously it will be and the result stored for future
## requests. If it has been calculated previously then the cached
## value will be returned.

cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()
	if (is.null(inverse)) {
		data <- x$get()
		inverse <- solve(data, ...)
		x$setInverse(inverse)
	}
	inverse
}
