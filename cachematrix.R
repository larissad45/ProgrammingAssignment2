## Matrix inversion is usually a costly computation and there may be some 
## benefits to caching the inverse of a matrix rather than compute it repeatedly.
## These pair of functions cache the inverse of a square matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inversa <- NULL
        set <- function(y) {
                x <<- y
                inversa <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inversa <<- inverse
        getinv <- function() inversa
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inversa <- x$getinv()
        if(!is.null(inversa)) {
                message("getting cached data.")
                return(inversa)
        }
        data <- x$get()
        inversa <- solve(data)
        x$setinv(inversa)
        inversa
}