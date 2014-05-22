## These functions return the inverse of a matrix. If this inverse has already been computed and cached
## and the matrix has not changed, the cached value is returned. These functiona assume that the matrix passed is 
## inverible


makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then the
## `cachesolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                 message("getting cached data")
        return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
