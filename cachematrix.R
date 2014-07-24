## Creates a matrix, solve the inverse of it and store it in the cache. When called for the second time it does not solve it but calls the cache matrix.

## Creates a "special" matrix and stores or "cache" the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## Request the value of x in makeCacheMatrix and calculates it and store it, or use the stored value.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s ## Return a matrix that is the inverse of 'x'
}

