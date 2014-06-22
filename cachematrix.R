## The functions makeCacheMatrix and cacheSolve computes and caches the invert of a matrix. makeCacheMatrix creates an object which makes it possible to store the inverse of a matrix. It returns a list of 4 functions; set and get a matrix 'x' and set and get the inverse of the matrix 'x'. cahceSolve computes the inverse of 'x' using makeCacheMatrix. If the inverse of 'x' has been computed earlier, the cached data is returned. 

## makeCacheMatrix creates an object to store the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL                                 # Initialize variable m for caching
             
        set <- function(y) {                      # set function: Store y in x. Reset m
                x <<- y
                m <<- NULL
        }
        
        get <- function() x                       # returns the matrix stored in x
        
        
        setInverse <- function(solve) m <<- solve # compute inverse of matrix using the build-in function solve.     
        getInverse <- function() m                # 'getInverse' returns m 
        
        
        list(set = set, get = get,                # Store the four functions in a list.
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve computes the inverse of matrix 'x' unless it has been computed earlier. If so, it returns the cached matrix

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        
        # test for cached data in m. If positive, return m
        if(!is.null(m)) { 
                message("getting cached data")
                return(m)
        }
        
        # Compute the inverse and store it in m
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        return(m)
}
