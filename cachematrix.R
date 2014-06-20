## makeCacheMatrix creates an object consisting of a list of functions for setting and getting a matrix x 
## and its inverse inv. x and inv are assigned in the global environment with the <<- operator

## cacheSolve takes as argument an object created with makeCacheMatrix, checks whether inv has a 
## cached value and returns it, otherwise calculates the inverse of x and assigns it to inv.


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## description of function: see above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
