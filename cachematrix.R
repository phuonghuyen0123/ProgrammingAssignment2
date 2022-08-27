##This function contains multiple sub functions that are used to store a matrix and cache its inverse
##The purpose of these functions are to: set the value of the matrix, get the value of the matrix, set the value of the inverse and 
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL

}
get <- function() x
        set_inverse <- function(inverse) i <<- inverse
        get_inverse <- function() i
        list(set = set,
             get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$get_inverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
         }
        data <- x$get()
        i <- solve(data, ...)
         x$set_inverse(i)
        i
}
