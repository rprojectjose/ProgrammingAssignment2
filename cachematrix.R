## Inverse of a Matrix

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( mtx = matrix() ) {
                    ## Initialize the inverse
    i <- NULL
                    ## setting the matrix
    set <- function( matrix ) {
        mtx <<- matrix
        i <<- NULL
    }
                    ## get the matrix
    get <- function() {
                    ## return the matrix
        M
    } 
                    ## set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
                    ## get the inverse of the matrix
    getInverse <- function() {
                    ## Return the inverse property
        I
    }    
                    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
                    ## "makeCacheMatrix" fix the matrix as the inverse of the special matrix
                    ## If the inverse matrix is in the cache (calculated) and has not changed,
                    ## then the "cachesolve" should retrieve it from the cache.
cacheSolve <- function(x, ...) {
                    ## Return a matrix that is the inverse of 'x'
    mtx <- x$getInverse()
                    ## return the inverse matrix if its already set
    if( !is.null(mtx) ) {
        message("getting matrix from cache")
        return(mtx)
    }
                    ## Get the matrix from our object
    data <- x$get()
                    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data
                    ## Set the inverse to the object
    x$setInverse(mtx)
                    ## Return the matrix
    mtx
}
