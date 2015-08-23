## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly.

## The following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix create a list of functions
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {

	in <- NULL 

	set <- function(y) {
	  
    ## assign value to an object in an environment different
	  ## from the current environment using "<<-" operator
		x <<- y 
		in <<- NULL 
	}

	get <- function() x
	setInverse <- function(inverse) in <<- inverse
	getInverse <- function() in

	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## The cacheSolve function returns the inverse of the matrix. It first checks if the inverse has been
## computed. If yes, it gets the result and skips the computation. If not, it computes the inverse
## and store the value in the cache using the setInverse.

cacheSolve <- function(x, ...) {
  
  ## assign inverse value to "in" via getInverse function in makeCacheMatrix
  ## function
	in <- x$getInverse
	
  ## check if the "in" has value in it (the inverse has been calculated)
	if(!is.null(in)) {
    
	  ## get the result and skips the computation
		message("Getting cached data")
		return(in)

	}
	
  ## if "in" has no value, then calculates the inverse
	data <- x$get
	in <- solve(data)
	
	## set the value of the inverse via "setInverse" function
	x$setInverse(in)
	
	## Return a matrix that is the inverse of 'x'
	in     
}
