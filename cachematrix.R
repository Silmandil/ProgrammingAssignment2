# These functions owe a lot to the example given in the programming assignment.
# The comments in some cases are tedious, but required for novices.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

    m <- NULL               # m should exist in this context.
    set <- function(y) {    # a function to set matrix and clear inverse.
        x <<- y             # assign x and m in their parent contexts.
        m <<- NULL
    }
    get <- function() x     # get, a simple function to get the matrix.
    setinverse <- function(inverse) m <<- inverse   # assign inverse in its parent context.
    getinverse <- function() m  # getinverse, a simple function to get the inverse.
    list(set = set, get = get,      # our list of functions
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {       # It wasn't null, so its known!
        message("getting cached data")
        return(m)
    }
    data <- x$get()     # Bring the data into this context
    m <- solve(data)    # The function that calculates the inverse is solve.
    x$setinverse(m)     # set the inverse (in a couple contexts up)
    m                   # return the inverse
}
