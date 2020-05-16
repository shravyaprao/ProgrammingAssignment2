## pair of functions that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object, a list with functions
##..to set value and get value of the matrix, and
##..to set value and get value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, 
	     get = get, 
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## cacheSolve computes the inverse of special "matrix" returned by makeCacheMatrix
##..if the inverse is already calculated (and matrix has not changed), 
##..then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	mymatrix <- x$get()
	m <- mean(mymatrix, ...)
	x$setinverse(inv)
	inv
}