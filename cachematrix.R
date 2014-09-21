## Two functions that are used to create a special object
## that stores a matrix and matrix's inverse cache

## Create an object to store the matrix and cache its inverse
## matrix <- makeCacheMatrix(rbind(c(1, 25, 3), c(40, 50, 60), c(7, 8, 9)))
## The first call of cacheSolve will calculate the inverse,
## solved <- cacheSolve(matrix)
## Further calls of cacheSolve will return the value cached during
## the previous call.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## cacheSolve - returns the inverse of the matrix stored in a
## "makeCacheMatrix". Further calls of cacheSolve for
## the same "makeCacheMatrix" structure will return a cached inverse.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
