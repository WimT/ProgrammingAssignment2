## These functions are part of assignment 2 of the R programming course of Johns Hopkins
## University on Coursera.org

## Author: Wim
## Version      Date            Description
## 0.1          2014-12-21      Initial version

## The function makeCacheMatrix encapsulates a regular square matrix so it can be used
## for caching the inverse
## The encapsulated "matrix" object has set and get functions to store and retrieve the
## matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setinv <- function(invrs) inverseMatrix <<-invrs
    getinv <- function() inverseMatrix
    list(set=set, get=get,
         setinv=setinv,
         getinv=getinv)
}


## The function cacheSolve return the inverse of an encapsulated square matrix that was created
## with the makeCacheMatrix function.
## If the inverse of the matrix was not yet calculated then it will do so and store the result.
## If the inverse was already calculated then it will return the previously calculated result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <-x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv<-solve(data, ...)
    x$setinv(inv)
    inv
}
