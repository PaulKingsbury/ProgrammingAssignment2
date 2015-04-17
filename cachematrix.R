## Put comments here that give an overall description of what your
## functions do

## creates a holder object for cached matrices.  
## Object holds both the matrix and functions for solving/inverting as well as returning the cached matrices
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # initialize m to NULL; this should get overridden the first time makeCache is invoked
    set <- function(y) { # this re-initializes the cache to NULL
        x <<- y
        m <<- NULL
    }
    get <- function() x # returns the original matrix
    setInverse <- function(solve) m <<- solve # passes m through to built-in solve function
    getInverse <- function() m # returns inverted matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) # set defaults to function
}


## solve the matrix
cacheSolve <- function(x, ...) { # requires input of makeCacheMatrix object
    ## Return a matrix that is the inverse of 'x', using a cached version if it is available
    m <- x$getInverse() # get the cached inversion within x 
    if (!is.null(m)) { #is the cache NULL or solved?  If cached, return the cache, otherwise solve and save to cache
        message("using cached inversion") # hey look, it's a debugging function!
        return(m) # I thought R didn't use explicit return statements?  Is this only to prevent further execution?
    }
    data <- x$get() # grab the content of the input matrix
    m <- solve(data, ...) # finally, we invert the matrix
    x$setInverse(m) # fill the cache with the inverted matrix
    m # return the inverted matrix also
}
