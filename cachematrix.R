## Coursera R-Programming
## Programming Assignment 2
##
## This file contains the functions:
##      makeCacheMatrix()
##      cacheSolve()
##
## They provide a mechanism to avoid unnecessary and time-comsumming operations 
## by caching the inverse of the input matrix. 
## The inversed matrix can be reused in repetitive computations, providing 
## the input data does not change.
## 
## Usage:
## m <- makeCacheMatrix(matrix(rnorm(9), 3, 3)) # input must be a square matrix
## cacheSolve(m)
## m$get() %*% m$getinverse()



## makeCacheMatrix()
## This function creates a special matrix object whose internal data and inverse are cached.

makeCacheMatrix <- function(x = matrix()) {
        x.inv <- NULL
        set <- function(y) {
                # TODO: check x is a square matrix?
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
## This function takes as input a matrix object created by makeCacheMatrix() and returns its inverse. 
## In case this computation is already done, the function returns the cached 
## value of the matrix inverse instead of repeating the computation. 

cacheSolve <- function(x, ...) {
        x.inv <- x$getinverse()
        if(!is.null(x.inv)) {
                message("getting cached data")
                return(x.inv)
        }
        data <- x$get()
        x.inv <- solve(data, ...)
        x$setinverse(x.inv)
        x.inv
}
