#These functions create an object type that can hold a invertible
#matrix in cache memory.

## This function creates the object-type of a matrix that is
## invertible. It follows the pattern we saw in the example
# with makeVector where we use the '<<-' operator to make
# the variables x, inv, and inverse accessable for other
# functions that call on makeCacheMatrix. The output is a list
# which the other function cacheSolve takes as input.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# This function takes the list output of makeCacheMatrix function
# and checks to see if the inverse needs to be re-calculated or
# whether it can use the cache inverse value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        }


