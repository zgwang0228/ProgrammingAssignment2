## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL #sets the value of m to NULL
    set <- function(y){
        # use '<<' to assign a value to an object in an 
        # environment different from the current environment
        x<<- y
        m<-- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m<<- inverse
    getinverse <- function() m
    list(set=set,get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m<- x$getinverse()
    
    # if the inverse has already been calculated
    if(!is.null(m)){
        # get it from cache and skips the computation
        message("getting cached data")
        return(m)
    }
    
    
    # otherwise, calculates the inverse
    data<- x$get() # run the get function to get the value of the input matrix
    m <- solve(data,...) # calculate the inverse of the input matrix
    x$setinverse(m) # run the setinverse function on the input matrix to cache it
    m # return the inverse
}
