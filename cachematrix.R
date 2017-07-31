## The following exemplifies caching by assigning via the <<- operator. Below are two functions, makeCacheMatrix() and cacheSolve() that are used to store a matrix, cache its inverse, and retrieve the inverse appropriately.

## The function makeCacheMatrix() creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the mean
# get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## The following function calculates the inverse of a matrix created with the above function, makeCacheMatrix().
# It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInv() function.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}

