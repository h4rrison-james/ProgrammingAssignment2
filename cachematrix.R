## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# Set the value of the matrix
# Get the value of the matrix
# Set the value of the inverse matrix
# Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes a special "matrix", that is really a list, and computes and sets the inverse matrix
# If the inverse matrix has already been cached, it will return the cached value

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Getting cached data for matrix inverse...")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
