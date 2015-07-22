## Caching the Inverse of a Matrix to reduce the time-consuming
## computations effect.


## MakeCacheMatrix creates a special "matrix" object that can cache its inverse.
## The special "matrix", which is a list containing a function to
## 1.set the matrix
## 2.get the matrix
## 3.set the inverse of the matrix
## 4.get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    TempMatrix <- NULL
    set <- function(y) {
        ## Use the '<<-' operator to assign a value to an object in an 
        ## environment different from the current environment.
        x <<- y
        TempMatrix <<- NULL
    }
    get <- function() x
    setInver <- function(Inverse) TempMatrix <<- Inverse
    getInver <- function() TempMatrix
    list(set = set, get = get, setInver = setInver, getInver = getInver)
}


## cacheSolve computes the inverse of the special "matrix" returned by  
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Check inverse of the matrix is exist or not.
    TempMatrix <- x$getInver()
    ## If the value is not null, display the message and return the matrix that 
    ## has been calculated.
    if(!is.null(TempMatrix)) {
        message("getting cached data")
        return(TempMatrix)
    }
    ## Get the matrix
    data <- x$get()
    ## Use 'solve' to Computing the inverse of a square matrix.
    TempMatrix <- solve(data, ...)
    ## Save the Computed result
    x$setInver(TempMatrix)
    ## Return a matrix that is the inverse of 'x'
    return(TempMatrix)  
}