## makeCacheMatrix defines a list of four functions to set/get a matrix or its inverse(see below)
## cacheSolve first looks if the  value of the inverse was already calculated and is available in the environment
## if it is not available, it is calculated and "stored" via the function setinv



## The function creates a list of 4 functions to set and get the matrix and its inverse 
## It is analogous to the makeVector function in the assignment instructions

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(calc_inv) inv <<- calc_inv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function looks up if the value was already calculated and stored in the cache/environment. If 
## yes, it returns the previously calculated value. If not, it calculates it and stores in the cache via setinv

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        if(!is.null(inv)){
                return(inv)
        }
        
        inv <- solve(x$get())
        x$setinv(inv)
        inv
        
        
        
        
        
}
