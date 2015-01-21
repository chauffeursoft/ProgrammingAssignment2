## Put comments here that give an overall description of what your
## functions do
## This pair of functions enables the creation and update of
## a matrix with a cached inverse

## Write a short comment describing this function
## Creates the matrix; the inverse is not computed and set to NULL
## (as of now)

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(i) inv <<- i
	getinv <- function() inv
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
## Returns the inverse of the matrix, using the cache
## if it is computed

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inv <-x$getinv()
	if(!is.null(inv)){
		message("getting cached inverse")
		return(inv)
	}
	mat <-x$get()
	inv <-solve(mat,...)
	x$setinv(inv)
	inv
}
