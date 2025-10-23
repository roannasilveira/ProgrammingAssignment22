## This function creates a matrix object that can cache its inverse.
## It returns a list of functions to set the matrix, get the matrix, set the 
## cached inverse and get the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # function to get the value of the matrix
    get <- function() x
    
    # function to set the value of the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    # function to get the value of the inverse
    getinverse <- function() inv
    
    # return a list containing the functions
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix().
## If the inverse has already been calculated and the matrix has not changed,
## it retrieves the inverse from the cache instead of recalculating it.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # if inverse not cached, compute
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)      
    inv
}