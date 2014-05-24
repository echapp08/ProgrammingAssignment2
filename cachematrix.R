##Intro to R Programming
##Programming Assignment 2

## makeCacheMatrix and cacheSolve are a pair of functions that generate and cache
## the inverse of a matrix. 


## makeCacheMatrix: 
##	This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        MatInv <- NULL
        set <- function(y) {
                x <<- y
                MatInv <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) MatInv <<- inv
        getinverse <- function() MatInv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##cacheSolve: 
##	This function computes the inverse of the special "matrix" returned by
## 	makeCacheMatrix above. If the inverse has already been calculated (and the
##	matrix has not changed), then the cachesolve should retrieve the inverse 
##	from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        MatInv <- x$getinverse()
        if(!is.null(MatInv)) {
                message("getting cached data")
                return(MatInv)
        }
        data <- x$get()
        MatInv <- solve(data, ...)
        x$setinverse(MatInv)
        MatInv
}
