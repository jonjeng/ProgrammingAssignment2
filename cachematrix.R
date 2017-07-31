## The following exemplifies caching by assigning via the <<- operator. Below are two functions, makeCacheMatrix() and cacheSolve() that are used to store a matrix, cache its inverse, and retrieve the inverse appropriately.

## The function makeCacheMatrix() creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the mean
# get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    # The inverse of the matrix has not been calculated yet so set the corresponding cache appropriately
    inv <- NULL
    
    # If the matrix is modified, its inverse has not been calculated yet so set the corresponding cache appropriately
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # The following function retrieves the defined matrix
    get <- function() x
    
    # The following function caches the inverse
    setInv <- function(inverse) inv <<- inverse
    
    # The following function retrieves the cached inverse
    getInv <- function() inv
    
    # The function returns a list of the functions previously defined
    list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## The following function calculates the inverse of a matrix created with the above function, makeCacheMatrix().
# It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInv() function.

cacheSolve <- function(x, ...) {
    # This function checks the appropriate cache. If the value of it is not null, it returns the value in the cache
    inv <- x$getInv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # If the cache value is null, the function retrieves the matrix, finds its inverse, caches the value, and returns said value.
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}

