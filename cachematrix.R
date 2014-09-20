## Goal of this program is to have two functions that 
## cache the inverse of a matrix in order to save resources

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        invmat <- function(invertedmatrix) m <<- invertedmatrix
        getinv <- function() m
        list(set = set, get = get,
             invmat = invmat,
             getinv = getinv)
}

## Computes the inverse of the special "matrix" returned by
## the fuction makeCacheMatrix created above. If the inverse
## has already ben calculated (and the matrix has not changed),
## then the following forumla will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$invmat(m)
        m
}
