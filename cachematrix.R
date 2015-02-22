## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly (there are also alternatives to 
## matrix inversion that we will not discuss here).
##
## The following pair of functions provide a mechanism to cache the inverse of a given matrix
## within the object containing said matrix. 

## This function creates a special "matrix" object that can cache its inverse.
## The object has specified functions for:
## - setting the matrix
## - getting the matrix
## - setting the inverse of the matrix
## - getting the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  ## inverse. if not calculated, equal to NULL
        set <- function(y) {
                x <<- y  ## new value for matrix
                inv <<- NULL  ## delete cached inverse
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve  ## sets new inverse
        getinverse <- function() inv  ## gets inverse value
        
        ## list of all available functions of the object
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache.
##
## NOTE: This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        ## use the object's function that returns the object's inverse
        m <- x$getinverse()
        
        ## if the value returned is not null, then the value is already computed and cached
        if(!is.null(m)) {
                ## print message notifying that the solution was already cached
                message("Using cached data")
                ## return value returned from object
                return(m)
        }
        
        ## get the matrix data of the object
        data <- x$get()
        
        ## Calculate the inverse of the given matrix
        m <- solve(data, ...)
        
        ## set calculated inverse matrix in object (from now on, it is cached within it)
        x$setinverse(m)
        
        ## return the calculated inverse matrix
        m
}
