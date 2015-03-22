## Put comments here that give an overall description of what your
## functions do

## We create a function which defines a list containing a function 
## to set the value of the vector, get the value of the vector, 
## set the value of the inverse and get the value of the inverse.
## We suppose that the matrix we receive is squared and can be inversed
## so the function solve with only one argument returns the inverse of the
## original matrix

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks if the inverse of the matrix has already been calculated
## and is cached. If it is, the function gets the inverse from the cache, if it
## isn't, it calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
