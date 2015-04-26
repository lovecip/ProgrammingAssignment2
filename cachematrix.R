## Usage : 
## 1. Create a matrix
## >m <- rbind(c(1,2),c(3,4))
## >m
## 2. Create a special matrix using makeCacheMatrix function
## >sm <- makeCacheMatrix(m)   
## 3. Return a matrix that is the inverse of m, using cacheSolve function
## >cacheSolve(sm)
## 4. Call cacheSolve function again to retrieve the inverse from cache. You should see 'getting cached data' message
## >cacheSolve(sm)

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix's inverse
## 4. get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {

	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list (set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}

## cacheSolve: This funtion computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse
## has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

	i <- x$getinverse()
	if (!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	
	## Computing the inverse of a square matrix can be done 	
	## with the solve function in R. For example, if X is a 	
	## square invertible matrix, then solve(X) returns its 	## inverse.

	x$setinverse(i)
	i
}
