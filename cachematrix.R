## These functions are used to compute the inverse of a matrix
## but to also cache the result, so that if it is required
## subsequently, it does not need to be recomputed.

## makeCacheMatrix() takes the source matrix as an argument and
## returns a 4-element list, where each element is a function:
##   set - sets the source matrix within the environment
##   get - gets the source matrix from the environment
##   set_inv - sets the inverse of the source matrix within the environment
##   get_inv - gets the inverse of the source matrix from the environment
## In each case the 'environment' is common to all four functions, is created
## when makeCacheMatrix() is called and is unique for each call of makeCacheMatrix()

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        get <- function() x
        set_inv <- function(solve) x_inv <<- solve
        get_inv <- function() x_inv
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}


## cacheSolve() takes a cacheMatrix, i.e. a list as returned by makeCacheMatrix
## and returns the inverse of the matrix originally provided.  If the inverse has
## previously been computed, it is retrieved from the cache.  If not, it is
## computed and stored in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inv <- x$get_inv()
        if(!is.null(x_inv)) {
                message("getting cached data")
                return(x_inv)
        }
        ## pull the original matrix from the list cache, solve it, then store it in the cache
        data <- x$get()
        x_inv <- solve(data, ...)
        x$set_inv(x_inv)
        x_inv
}