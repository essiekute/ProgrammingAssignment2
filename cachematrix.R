## A pair of functions that cache the inverse of a matrix


## creates a special matrix object with the ability to cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

	## Initialize the inverse property
    i <- NULL

    ## Setting the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## getting the matrix
    get <- function() {
    	## returning the matrix
    	m
    }

    ## Setting the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## The inverse of the matrix
    getInverse <- function() {
        ## Return the property of the inverse
        i
    }

    ## list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ##  return the inverse again if it has been set    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Getting matrix from object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}
