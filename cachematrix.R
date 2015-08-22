## Utilizes caching to avoid repeated computation of inverse of a matrix,
## since it is resource-intensive. Uses two functions makeCacheMatrix and cacheSolve

## makeCacheMatrix function: Creates a list to
## set the value of the matrix
## get the value of the matrix
## set the value of inverse of the matrix
## get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

## cacheSolve function: 
## computes the inverse of the matrix 
## first, will check if inverse is already computed
## and returns the results rather than computing again.
## if not computed, will compute the inverse and 
## assign to setinverse function

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
        
}
