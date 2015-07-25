## Below pair of functions that cache the inverse of a matrix
## makeCacheMatrix and cacheSolve

## makeCacheMatrix function takes matrix as an argument and does the following.
## set the value of the matrix
## get the value of the matrix
## set Inverted value of the matrix
## get Inverted value of the matrix

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    
    get <- function() x
    
    setInvert <- function(inv) im <<- inv
    
    getInvert <- function() im
    
    list(set = set, get = get, setInvert = setInvert, getInvert = getInvert)
}


## cacheSolve function returns the inversion of the matrix
## from cache if it exists otherwise calculates the inversion, caches it and returns the same.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getInvert()
    
    if (!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    
    data <- x$get()
    
    im <- solve(data)
    
    x$setInvert(im)
    
    im
}
