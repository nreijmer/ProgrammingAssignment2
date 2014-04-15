## These functions are used to create the inverse of a given matrix and cache this matrix

## The function makeCacheMatrix stores a given matrix (x)
## And it makes it possible to store (using setInvert) and retrieve (using getInvert) the stored the inverse of this matrix
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setInvert <- function(inverted) m <<- inverted
        getInvert <- function() m

		list(set = set, get = get,
             setInvert = setInvert,
             getInvert = getInvert)
}


## This function is called to get the inverse of the matrix
## First the function checks if the invert is already available in x
## If it is, the stored (cached) value is returned
## If it is not than the matrix is retrieved, the inverse determined and the inverse is stored (using setInvert) and returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getInvert()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
		data <- x$get()
		m <- solve(data, ...)
		x$setInvert(m)

        m
}
