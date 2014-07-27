## These two functions allow storing a matrix and its 
## inverse. It follows a lazy approach, so the inverse
## is not calculated until it is required for the very
## first time. Once the inverse has been calculated (and
## because such a calculus is time consuming) the result
## is stored, so any further requirement to calculate
## the inverse will return the already calculated one
## instead of calculating it again.
## If the matrix stored is changed, then the cached inverse
## is removed (cleared) to avoid returning data 
## corresponding to the previous matrix.
## NOTE: Please, do not use exactly singular matrices.

## This function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve <- function() s
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}

