## "Caching the Inverse of a Matrix" Programming Assignment 2: Lexical Scoping (r-programming, week 3)
## 
## makeCacheMatrix
## cacheSolve

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        cached <- NULL

        set <- function(y) {
                x <<- y
                cached <<- NULL
        }
        
        get <- function() x
        
        setsolve <- function(solve) cached <<- solve
        getsolve <- function() cached
        
        list(set = set, get = get, 
                setsolve = setsolve,
                getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
        cached <- x$getsolve()
        
        # is there any matrix cached?
        if(!is.null(cached)) {
                return(cached)
        }
        
        # if there is no matrix cached
        matrix2cache <- x$get()
        cached <- solve(matrix2cache)
        x$setsolve(cached)
        cached
}


