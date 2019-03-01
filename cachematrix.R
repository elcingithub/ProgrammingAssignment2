## makeCacheMatrix Function calculates and stores inverse of a matrix.
## cacheSolve Function retrieves the cached data

##  makeCacheMatrix will get the inverse matrix and store it

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


## Function returns the inverse of the matrix

cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
