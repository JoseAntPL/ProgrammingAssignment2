## Functions that store a matrix and its inverse in cache, 
## makeCacheMatrix stores the matrix in cache and initializes ts inverse as NULL. 
## cachesolve checks if there is an inverse value stored for a cached matrix 
## and returns it or calculates it and stores it.

## creates a "matrix" that can get and set in cache the
## values of itself and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL
    set <- function(y) {
        x <<- y
        inv_m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv_m <<- inverse
    getinv <- function() inv_m
    list(set = set, get = get, setinv = setinv,
         getinv = getinv)
}


## Checks if the value of the inverse matrix is stored in cache, 
## if it is returns that value, if not calculates it using solve and then stores it

cacheSolve <- function(x, ...) {
        inv_m <- x$getinv()
        if(!is.null(inv_m)) {
            message("getting cached data")
            return(inv_m)
        }
        data <- x$get()
        inv_m <- solve(data, ...)
        x$setinv(inv_m)
        inv_m
}
