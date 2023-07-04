## The following functions calculate inverse of a matrix, after a quick check
## to verify if it wasn't already calculated

## makeCacheMatrix creates a list with four differents parameters: 
## set - insert NULL in i; get - insert the matrix in get;
## setinverse - insert inverse in i; getinverse - get inversed matrix

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


## This function calculates inverse of a matrix after a check, if it wasn't
## already calculated. This function considers every matrix "invertible"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}
