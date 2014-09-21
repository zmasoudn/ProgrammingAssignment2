## The following two functions cache the inverse of a matrix

## makeCacheMatrix: creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvers <- function(invers) m <<- invers
        getinvers <- function() m
        list(set = set, get = get,
             setinvers = setinvers,
             getinvers = getinvers)
}

## cacheSolve: computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvers()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvers(m)
        m
}



