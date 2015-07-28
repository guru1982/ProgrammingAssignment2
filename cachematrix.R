## makeCacheMatrix will create a 'matrix object', a list containing 4
## functions - for getting and setting a matrix, and for getting and
## setting its inverse.
##



makeCacheMatrix <- function(x = matrix()) {
    z <- NULL
    setmatrix <- function(y = matrix()){
        x <<- y
        z <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(s = matrix()){
        z <<- s
    }
    getinverse <- function() z

    list(set = setmatrix, get = getmatrix, setinverse = setinverse, getinverse = getinverse)

}


##cachesolve will take the matrix object as its argument. it is for
##calculating the inverse of the matrix included in the argument. it
##checks if the inverse is already present. if NULL, it calculated the
##inverse by using the solve function. if present, it will print out
##the cached value.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    solinv <- solve(data)
    x$setinverse(solinv)
    solinv
}
