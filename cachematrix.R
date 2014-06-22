## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list of four functions; set, get, setInverse and getInverse.

makeCacheMatrix <- function(x = matrix()) {
        # Initialize variable m for storing the inverse computation
        m <- NULL
        
        # 'Set' function: takes the input matrix y and stores it in a variable, x. Resets m
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # 'get' function: prints the matrix stored in x
        get <- function() x
        
        # 'setInverse' function: compute the inverse of a matrix using the build-in function solve. Store the result in m
        setInverse <- function(solve) m <<- solve
        # 'getInverse' function: print m 
        getInverse <- function() m
        
        # Store the four functions in a list. The list is the result of the makeCacheMatrix function
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        
        # test if m is different from NULL. If so, return m in which the inverse of the matrix, x, is already stored
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # Compute the inverse of the matrix and store it in m
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        return(m)
}
