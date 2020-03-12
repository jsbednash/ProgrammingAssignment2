## Together, these functions create a matrix object that stores
## a matrix and caches its inverse. 

## makeCacheMatrix creates a list containing 4 functions.
## set and get functions set and get matrices.
## setinverse and getinverse functions set and get the inverse of 
## matrices.

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

## cacheSolve first checks to see if the inverse of a matrix has 
## been calculated. If not, it determines the inverse and caches 
## the solution using the setinverse funciton.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        m1 <- x$get()
        m <- solve(m1, ...)
        x$setinverse(m)
        m
}