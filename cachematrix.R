## Put comments here that give an overall description of what your
## functions do

## This function returns a list where each element in the list is a closure.

# 1. set (sets the matrix to be inverted and assigns the attribute caching the inverted result to NULL so that if the matrix is changed
#           the inverse is calculated anew.)
# 2. get (returns the original matrix(x))
# 3. setinverse (caches the result of an operation on the matrix)
# 4. getinverse (returns the inverse)

#

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Given a list of closures, calculate and store the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

