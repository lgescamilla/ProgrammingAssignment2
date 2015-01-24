## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## The following two functions help to calculate the inverse and cache it

makeCacheMatrix <- function(x = matrix())
## This function creates a special "matrix" object that can cache its inverse
{
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
} ## End of makeCacheMatrix


## Write a short comment describing this function

cacheSolve <- function(x, ...)
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve retrieves the inverse from the
## cache without re-calculating
{
    inv <- x$getinverse()
    if(!is.null(inv))
    {
        message ("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
} ## cacheSolve
