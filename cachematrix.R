## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## x is a square invertible matrix
        ## It returns a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()
        
        inverse = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inverse <<- NULL
                
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inverse
        list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        # x: output of makeCacheMatrix()
        # return: inverse of the original matrix input to makeCacheMatrix()
        
        inverse = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inverse)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inverse)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inverse = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inverse)
        
        return(inverse)
}
