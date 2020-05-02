
## The  makeCacheMatrix creates a special "vector", which is really a list containing a 
#function to
#1. Set the value of matrix "x"
#2. Get the value of the matrix
#3. Set the inverse matrix
#4. Get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        x
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function calculates the inverse of the matrix created with the 
#above function. However, it first checks to see if the inverse matrix has already been 
#calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, 
#it calculates the inverse of the data and sets the value of the inverse in the cache via 
#the setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}


