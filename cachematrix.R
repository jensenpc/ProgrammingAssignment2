## The functions, makeCacheMatrix and cacheSolve, calculate the inverse of
## non-singular, square matrices. Calculating the inverse of a matrix is
## computationally expensive; these functions address the issue by caching
## solutions of previously calculated inverse matrices.


## The first function, makeCacheMatrix, takes a matrix, X, as its input argument
## and returns a list of functions to:
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the value of the inverse matrix
## (4) get the value of the inverse matrix

makeCacheMatrix <- function(X = matrix()) {
        inverse <- NULL
        set <- function(Y) {
                X <<- Y
                inverse <<- NULL
        }
        get <- function() X
        setInverse <- function(solve) inverse <<- solve
        getInverse <- function() inverse
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## The second function, cacheSolve, takes the output of makeCacheMatrix and 
## returns X^-1, the inverse matrix of X. If X^-1 has been previously 
## calculated, cacheSolve returns the cached solution rather than recalculating
## the inverse matrix.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
