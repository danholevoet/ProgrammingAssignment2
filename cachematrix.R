## These functions allow you to repeatedly find the inverse of a matrix without
## repeating previously computed operations.

## This function provides a special matrix that can be inverted repeatedly
## without recalculation.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function inverts a special matrix (see makeCacheMatrix). If the inverse
## has been previously calculated, it uses the previously computed value.
## Otherwise, it calculates the inverse and saves it.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
