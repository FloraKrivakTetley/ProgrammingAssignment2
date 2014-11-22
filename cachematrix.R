## This pair of functions was written for the Coursera R Programming Course for
## the Week 2 Programming Assignment.
## It allows you to cache the inverse of a matrix during a series of computations
## to avoid repeating the potentially slow process of computing it multiple times.

## This function creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function()x
	setinverse <- function(solve) m <<- solve
	getinverse <- function()m
	list(set=set, get=get, 
		setinverse=setinverse,
		getinverse=getinverse)
}


## This function computes the inverse of the special matrix calculated by makeCacheMatrix above

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
