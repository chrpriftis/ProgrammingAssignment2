## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## invmatrix shall hold the inversed matrix
    invmatrix <- NULL
    set <- function(y) {
        ## x shall hold the initial matrix 
        x <<- y
        invmatrix <<- NULL
    }
    ## get Shall return the original matrix
    get <- function() x
    ## setinv shall load the passed inversed matrix to invmatrix
    setinv <- function(invertedmatrix) invmatrix <<- invertedmatrix
    ## getinv shall return the inversed matrix (held in invmatrix)
    getinv <- function() invmatrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invmatrix <- x$getinv()
    if(!is.null(invmatrix)) {
        message("getting cached data")
        return(invmatrix)
    }
    data <- x$get()
    ## solve shall calculate the inversed matrix
    invmatrix <- solve(data)
    ## set invmatrix to hold the inversed matrix
    x$setinv(invmatrix)
    ## return invmatrix
    invmatrix    
}
