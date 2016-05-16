#These functions are intended to create a matrix and allow for its inverse to be stored in a cache,
#as well as checks if the inverse has already been calculated and stored in the cache

## This function makes a cache-able matrix that can later be inverted and have the inverse stored in the cache

makeCacheMatrix <- function(x=matrix()){
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}

#This checks if the cacheable matrix made by makeMatrixCache has already been cached. If not, it
# solves for the inverse. If it IS cached, it just prints the cached data.

cacheSolve <- function (x, ...){
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