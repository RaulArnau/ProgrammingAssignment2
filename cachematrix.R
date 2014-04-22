## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix()
## This function creates a special matrix object whose internal data and inverse are cached.

makeCacheMatrix <- function(x = matrix()) {
        # check x is a square matrix
        if (!is.matrix(x)){
                warning("'x' should be a matrix")
                return(NA)
        }
        if (dim(x)[1] != dim(x)[2]){
                warning("'x' must be square")
                return(NA)
        }
        x.inv <- NULL
        set <- function(y) {
                # check x first
                x <<- y
                x.inv <<- NULL
        }
        get <- function() { x }
        setinverse <- function(inverse) {
                x.inv <<- inverse
        }
        getinverse <- function() {
                x.inv
        }
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve() 
## This function calculates the inverse of a matrix object created by makeCacheMatrix().
## In case this computation is already done the function returns the cached value of the matrix inverse in order to avoid unnecessary computational load

cacheSolve <- function(x, ...) {
        invx <- x$getinverse()
        if(!is.null(invx)) {
                message("getting cached data")
                return(invx)
        }
        data <- x$get()
        invx <- solve(data, ...)
        x$setinverse(invx)
        invx
}
