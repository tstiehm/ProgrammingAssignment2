## Put comments here that give an overall description of what your
## functions do

## These functions take a matrix and calculate it's inverse. They preserve the 
## inverse so that you can retrieve it multiple times without recalculating it.
## Usage Example:
## > original<-matrix(c(2,8,4,7,-6,4,2,9,1),3,3)
## > original
## > inv_orig<-makeCacheMatrix(original)
## > cacheSolve(inv_orig)

## Write a short comment describing this function
## This function creates a set of functions that get and set a matrix and its
## inverse
makeCacheMatrix <- function(x = matrix()) {
    ## set the inverse to NULL initially
    m <- NULL
    ## create function to set matrix
    set <- function(y) {
        ## set the matrix
        x <<- y
        ## NULL the inverse when the matrix is set
        m <<- NULL
    }
    ## create function to get the matrix
    get <- function() x
    ## create function to calculate the inverse value and set it
    setsolve <- function(solve) m <<- solve
    ## create function to get inverse value
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## Write a short comment describing this function
## fucntion returns the value of the cached inverse or calculates it and 
## returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    ## in inverse is cached return it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## inverse isn't cached so calculate, set and return
    data <- x$get()
    m <- solve(data, ...)
    x$set(m)
    m
}
