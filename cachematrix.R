## Programming Assignment 2: Lexical Scoping
## Demonstrates caching ability using lexical scoping to define variables

## makeCacheMatrix is a function which takes matrix x as an argument,
## then stores a list consisting of inv, set, get, setinverse, and getinverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is a function which takes x as an argument. x contains
## the environments as defined in makeCacheMatrix including the matrix itself.
## The function checks for cached inverse solution. If cache is empty (null),
## it calculates the inverse of the matrix. Otherwise it returns the cached data.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}