#### create a vector-object with 4 methods
#### variable x, inv manipulated within the object's environment

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    Set <- function(y){
        x <<- y
        inv <<- NULL
    }
    Get <- function() x
    SetInverse <- function(ii) inv <<- ii
    GetInverse <- function() inv
    # return an object which can use those 4 internal fucntions
    list(set=Set,
         get=Get,
         setinverse=SetInverse,
         getinverse=GetInverse)

}


## This function will return a matrix that is the inverse of the input matrix
## If the inverse of the input matrix has been calculated prior to calling cacheSolve(x), i.e. the inverse is cached
## the function will return the cached value for the matrix's inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(is.null(inv)){
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
    }
    else{  ## tell the learner about the source of the returned value
        message("getting cached data")
    }
    inv
}

## test the above code
#testmat <- matrix(data=rnorm(9),nrow=3,ncol=3)
#testmat
#solve(testmat)
#testmat_c <- makeCacheMatrix(testmat)
#cacheSolve(testmat_c)
#testmat_c$get()
#testmat_c$getinverse()
