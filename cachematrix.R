## cachematriz.R
## 

## Create a list of functions for get, set the x matrix and get and set the 
## inverse of the x matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Calculate the inverse of x if there is no in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}
