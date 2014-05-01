## Put comments here that give an overall description of what your
## functions do

## creating a special "vector", which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
## ivm indicates the inverse of the matrix
## exivm indicates ivm which already exists

makeCacheMatrix <- function(x = matrix()) {
        ivm <- NULL
        set <- function(y) {
                x <<- y
                ivm <<- NULL
        }
        get <- function() x
        setivm <- function(exivm) ivm <<- exivm
        getivm <- function() ivm
        list(set = set, get = get,
             setivm = setivm,
             getivm = getivm)
}

## calculating the inverse of the matrix based on the above function. 
## It first checks to see if the ivm has already been calculated. 
## If so, it gets the ivm from the cache and skips the computation. 
## Otherwise, it calculates the ivm of the data and sets the value of 
## the ivm in the cache via the setivm function.

cacheSolve <- function(x, ...) {
        ivm <- x$getivm()
        if(!is.null(ivm)) {
                message("getting cached data")
                return(ivm)
        }
        data <- x$get()
        ivm <- solve(data, ...)
        x$setivm(ivm)
        ivm
}
