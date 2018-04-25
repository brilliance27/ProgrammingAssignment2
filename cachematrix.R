## makeCacheMatrix and cacheSolve cache the inverse of the input
## matrix x

## makeCacheMatrix cache the inverse, and creat a list to store it

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


## call the function to get the cache. and use message to show it
## whether it is the first call or getting the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        message("it is calculated data")
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
