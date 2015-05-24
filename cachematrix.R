## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Here we create functions for cahing the matrix
makeCacheMatrix <- function(x = matrix()) {
    ##initialized inversed matrix for newly uploaded matrix
    ##If we've just uploaded the matrix we have no computed inversed ones
    inv <- NULL
    #here we defines setters and getters for the matrix
    #set the value of new matrix object (including assigning null for inversed matrix after updates of initial one)
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    #Define function for getting the cashed matrix
    get <- function() x
    ##here we define function for setting value of inversed matrix
    setinv <- function(solve) inv <<- solve
    ##function below return us inversed matrix that is stored in cashe
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## Write a short comment describing this function
##This function check do we have already cached inverse matrix and returned it if any
##otherwise function calculate inversed matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## get cached value from matrix object
    inv <- x$getinv()
    ## check if cached vakue is null
    if(!is.null(inv)) {
        ## if cached value of inversed matrix is not null return already existing inversed matrix
        message("getting cached data")
        return(inv)
    }
    ##get matrix data
    data <- x$get()
    ##compute inversed matrix
    inv <- solve(data, ...)
    ##Cache inversed matrix
    x$setinv(inv)
    ##just returning result
    inv
}