## These functions return the inverse of a matrix. If this inverse has already been computed and cached
## and the matrix has not changed, the cached value is returned. These functions assume that the matrix passed is 
## invertible


makeVectorCache<- function(x = numeric()) { ## Build a list of functions for managing cached results
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then the
## `cachesolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
                        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        }
}
