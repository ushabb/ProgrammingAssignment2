## 'cacheSolve' returns the inverse of input matrix either by;
## 1. Solving using solve function - if the operation is carried out for the first time
## 2. Simply retrieving from cache using getters and setters(list elements) from 'makeCacheMatrix' 



## 'makeCacheMatrix' creates a special "matrix" object that can cache 
## its inverse and makes it accessible through getters and setters(returned as list elements)

makeCacheMatrix <- function(x = matrix()) {
		matrixInverse <- NULL
		
		set <- function(y){
				x <<- y
				matrixInverse <- NULL
				}
		get <- function() x
		
		setinverse <- function(solveInverse) matrixInverse <<- solveInverse
		getinverse <- function() matrixInverse
		
		list (set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## 'cacheSolve' calculates the inverse of the input matrix using
## solve function if the operation is done for the first time and the
## inverse is cached, on subsequent operations it simply retrieves the  
## inverse function from cache

cacheSolve <- function(x, ...) {
		matrixInverse <- x$getinverse()
		
		if(!is.null(matrixInverse)) {                 
		message("--getting cached data--")
		return(matrixInverse)
		}
		inverseData <- x$get()                               
		matrixInverse <- solve(inverseData, ...)
		x$setinverse(matrixInverse)
		
        ## Returns a matrix that is the inverse of 'x'
		matrixInverse
}
