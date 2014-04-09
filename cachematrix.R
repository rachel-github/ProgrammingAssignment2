## makeCacheMatrix creates a special "matrix" that is actually a list
## containing the following functions
## 1. set(y)              - sets the value of the matrix to the value of y
## 2. get()               - returns the value of the matrix
## 3. setinverse(inverse) - sets the value of the cached matrix inverse to 
##                          the value of inverse
## 4. getinverse()        - returns the value of the cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
    	x <<- y
    	inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve computes the inverse of the special "matrix"
## created using makeCacheMatrix.  Before it computes the inverse 
## using solve(),  it first checks if the inverse has 
## already been computed previously.  If so, it gets the inverse 
## from the cache (via getinverse()) and skips the computation.  
## Otherwise, it calculates the inverse of the matrix and sets
## the value of the inverse to the cache (via setinverse()).

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
