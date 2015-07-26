## makeCacheMatrix stores and returns a matrix and cacheSolve either calculates
## or returns from cache inverse of the earlier stored matrix 

#stores and returns a quadratic investable matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(B) {
                x <<- B
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- solve(x)
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## either returns from cache or calculates inverse of the matrix stored by 
## makeCacheMatrix function

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
        ## Return a matrix that is the inverse of 'x'
}
