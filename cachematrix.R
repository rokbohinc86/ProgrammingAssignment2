## Below I have created two functuions, makeCacheMatrix and cacheSolve. makeCacheMatrix 
## caches the inverse of a matrix, while cacheSolve returns the inverse of a matrix
## accesing the cached inverse if the inverse of that matrix was already calculated before 

## makeCacheMatrix takes as the input a matrix "m", sets the inv to NULL and returnes 
## functions "get", "setinv" and "gitinv". "get" gets the original matrix "m", "getinv" gets
## the value "inv", and "setinv" sets "inv" to a given value.

makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    get <- function() m
    set <- function(y) {
        m <<- y
        inv <<- NULL
    }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes as input the output of the function makeCacheMatrix "x". Then
## it sets "inv" to the returned value of x$getinv(). The first time cacheSolve
## is called on an input "x" x$getinv() should return NULL. In this case x$get()
## gets the matrix "m" from "x" and then caluclates the inverse solve(m) (here I
## assume the matrix "m" is invertable). Afterwards I cache the value of the inverse
## "inv" with the function "x$setinv(inv)" and return the inverse "inv". If 
## cacheSolve was already called on an input "x" the if statement should be true
## because we previously set "inv" to a value different from NULL. The the message
## "getting cached data" should be returned and the function should exit returning "inv"

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    m <- x$get()
    inv <- solve(m)
    x$setinv(inv)
    inv
}
