## The two functions create (makeCacheMatrix) and invert (cacheSolve) a matrix. The matrix
## created by makeCacheMatrix is special in a way that it is able to store its inverse in the cache
## and return it instead of calculating it anew each time.

## makeCacheMatrix <- function(x = matrix())
## The function is a wrapper for a matrix it takes as its argument. It adds object-like functionality
## to the matrix: it can return its value or alter its contents, assign to it an inversed matrix 
## and return it.
## the function takes a generic matrix as an argument (x) and returns a list of functions that 
## can be performed on the matrix.
## Examples:
## myMatrix <- makeCacheMatrix(rnorm(100,1),10,10) - creates a wrapper over a 10x10 matrix of normaly
## distributed values.
##
## m <- myMatrix$get() - returns the initial matrix
##
## myMatrix$setinverse(inversed_matrix) - assigns a inversed matrix to the initial matrix
## (to be used only by the cacheMatrix() function)
##
## i <- myMatrix$getinverse() - returns the stored inverse of the initial matrix
## (or NULL it the cache is empty)

makeCacheMatrix <- function(x = matrix()) {
        inversed <- NULL
        set <- function(y) {
                x <<- y
                inversed <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) inversed <<- inverse
        
        getinverse <- function() inversed
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve <- function(x, ...)
## The function takes as an argument a special matrix cread with the makeCachMatrix() function
## and returns its inverse. First it checks if the matrix has it inverse already stored it the cache,
## in which case it reads it from there, otherwise the inverse is calculated with the solve() function.
## The function also checkes if the given matrix is invertible (the deterinant is not equal to 0)
## and in case it is not, stops with the message = "the matrix is non invertible (determinant is equal to 0)"

cacheSolve <- function(x, ...) {
        inversed <- x$getinverse()
        if(!is.null(inversed)) {
                message("getting cached data")
                return(inversed)
        }
        data <- x$get()
        if(det(data) == 0) stop("the matrix is non invertible (determinant is equal to 0)")
        inversed <- solve(data)
        x$setinverse(inversed)
        inversed
}
