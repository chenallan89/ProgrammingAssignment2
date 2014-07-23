## makeCacheMatrix function creates a special matrix object that can cache its inverse
## cacheSolve function computes the inverse of the special matrix returned by the above function. If the inverse has already been calculated, then the cacheSolve function retrieves the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
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
