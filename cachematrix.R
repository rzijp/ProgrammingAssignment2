## Put comments here that give an overall description of what your
## functions do

## function makeCacheMatrix returns a list of functions that provide 
## cache capabilities to input variable x
makeCacheMatrix <- function(x = matrix()) {
    
    # Define a cache variable that is empty
    invCache <- NULL
    # Define function to set the value of x and to clear the cache
    set <- function(y) {
        x <<- y
        invCache <<- NULL
    }
    # Define the function to get the value of x, simply returning x
    get <- function() x
    # Define function to fill the cache
    setinv <- function(inv) invCache <<- inv
    # Define function to get the cache content
    getinv <- function() invCache
    # Return list of functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function cacheSolve returns the inverse of input matrix x (assumption 
## that x is always invertible). The inverse is cached if not done so
## before, and if already cached this cache is returned directly and the 
## inverse calculation is skipped

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # get the cached inverse of x
    inv <- x$getinv()
    # If the obtained cached is filled, return the cache
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # Apparently the cache was empty, now calculate the inverse
    data <- x$get()
    inv <- solve(data, ...)
    # Store the calculated inverse in the cache
    x$setinv(inv)
    # Return the inverse of x
    inv
}
