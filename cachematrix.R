## Sonia Smith - RPROG6 - Programming Assignment 2
## These functions cache a matrix to the global environment and calculates it inverse which is also cached globally.

## The makeCacheMatrix function create a matrix object, setter method, getter method, and method 
## get and set inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() {
        x
    }
    
    setSolve <- function(solve) {
        inverseMatrix <<- solve
    }
    
    getSolve <- function() {
        inverseMatrix
    }
    
    list(set = set, 
    get = get,
    setSolve = setSolve,
    getSolve = getSolve)
}


## The cacheSolve function determines if mat
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getSolve()
    
    if(is.null(s)) {
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)    
    }        
    
    s
}
