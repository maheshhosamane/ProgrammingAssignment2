## These functions are used to demonstrate how to write functions that cache the 
## inverse of a matrix and can be used to save the time taken to calculate the 
## inverse of a matrix when repeatedly used

## Usage example:
## > xmatrix <- matrix(c(2,2,3,2),nrow=2)
## > smat <- makeCacheMatrix(xmatrix)
## > cacheSolve(smat)
##      [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
## > cacheSolve(smat)
## getting cached data
##      [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
## > 

## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse of the matrix
##4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ## This function creates a special matrix
        i <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             getinv = getinv,
             setinv = setinv)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse in the cache via the `setinv`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        matrixdata <- x$get()
        i <- solve(matrixdata, ...)
        x$setinv(i)
        i
}
