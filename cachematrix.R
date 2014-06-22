## The functions makeCacheMatrix and cacheSolve computes and caches the invert of a matrix. makeCacheMatrix returns a list of 4 functions; set and get a matrix 'x' and set and get the inverse of the matrix 'x'. cahceSolve computes the inverse of 'x' and cahces the result. If the inverse of 'x' has been computed earlier, the cached data is returned. 

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


## cacheSolve computes the inverse of matrix 'x' unless it has been computed earlier. If so, it returns the cached matrix

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
