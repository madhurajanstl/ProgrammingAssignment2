## Functions to compute a matrix inverse and cache it.

## This function takes a square matrix as input an returns a list of four functions
## to set the matrix, get the matrix, caches the inverse matrix and retrieves the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(invM) inv <<- invM
        getinverse <- function() inv
        list(set = set, get=get, setinverse = setinverse,getinverse=getinverse)
}


## This function takes the output of makeCacheMatrix as input, and retrieves the matrix inverse from the cache, if exists. Else computes the inverse of the matrix
## and caches the inverse and returns the matrix inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        #get the inverse from the cache if exists
        if (!is.null(inv)){
                print("retrieving cached matrix inverse")
                return(inv)
        }
        #else compute the inverse
        y <- x$get()
        inv <- solve(y, ...)
        x$setinverse(y)
        return(inv)
}
