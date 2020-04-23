## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x=matrix()) {
    iv <- NULL
    set <- function(y) {
        x <- y
        iv <- NULL
    }
    get <- function() x
    setinv <- function(new_inv) iv <- new_inv
    getinv <- function() iv
    list(set=set, get=get, getinv=getinv, setinv=setinv)
}


cacheSolve <- function(x, ...) {
    iv <- x$getinv()
    if (!is.null(iv)) {
        message('Cache inverse is used!')
        return(iv)
    }
    data <- x$get()
    iv <- solve(data) %*% data
    x$setinv(iv)
    iv
}
