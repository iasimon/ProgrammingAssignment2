## The below functions cache the inverse of a matrix rather than compute it
## repeatedly.

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse. The special "matrix" is really a list of functions.

## makeCacheMatrix function initializes the list object with the original matrix
makeCacheMatrix <- function(x = matrix()) {
        ## on initialization the value of the cached inverse is set to NULL
        i <- NULL
        ## set function updates the original matrix in an existing list object
        set <- function(y) {
                x <<- y
                ## on update the value of the cached invesre is set to NULL
                i <<- NULL
        }
        ## get function returns the original matrix
        get <- function() x
        ## setsolve function updates the value of the cached inverse
        setsolve <- function(solve) i <<- solve
        ## getsolve function returns the value of the cached inverse
        getsolve <- function() i
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the "matrix" has not been updated), then the cachesolve retrieves the
## inverse from the cache.

## cachesolve function takes the list object as argument
cacheSolve <- function(x, ...) {
        ## the value of the inverse is NULL if the inverse has not been
        ## calculated since the initiation of the list object or the last update
        ## of the original matrix in the list object
        i <- x$getsolve()
        ## if the cached inverse is not NULL, the cached inverse is returned
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## if the cached inverse is NULL, the original matrix is retreived, the
        ## inverse is calculated, stored in the list object and printed
        data <- x$get()
        i <- solve(data)
        x$setsolve(i)
        i
}
