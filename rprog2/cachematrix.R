## In summary, these two functions work to cache the inverse of a given matrix.
## This first function, titled "makeCacheMatrix," will create a matrix object 
## that can cache its inverse. It contains a function that will set the values 
## within the matrix, get the values within the matrix, set the values of the 
## inverse of the matrix, and get the values of the inverse of the matrix.
## Function 1
## initialize
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}


## This second function, titled "cacheSolve," will first determine whether or not 
## the inverse was already calculated in the first function. If it has, then the 
## cached inverse will be retrieved. If a new matrix was supplied and the inverse 
## has not yet been calculated, then both functions will be used to calculate the 
## new inverse and will set that new inverse in the cache.
## Function 2
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
    m
}
