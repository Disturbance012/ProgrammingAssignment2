## These functions are used to cache the inverse of a matrix, so that it need not be computed repeatedly
##(as matrix inversion is costly computation wise)

## This function creates a matrix object which will then be used to store its inverse in cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## Making use of the first function, this function computes a matrix's inverse
## If the inverse is already calculated, it retrieves it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Getting cached Inverse Matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
