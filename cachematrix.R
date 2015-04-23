## makeCacheMatrix function creates a special matrix object that can cache its inverse. It uses 
## set function to set a matrix to an object using makeCacheMatrix function,
## get function to return stored matrix,
## setInv function to set the inversed matrix
## getInv function to get the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        xInv <- NULL
        set <- function(y) {
                x <<- y
                xInv <<- NULL
        }
        get <- function() x
        setInv <- function(inv) xInv <<- inv
        getInv <- function() xInv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## CacheSolve function return the cached inverse matrix of given matirx from memory
## else it inverse the matrix, cache it in memory and returns it

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting from cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
