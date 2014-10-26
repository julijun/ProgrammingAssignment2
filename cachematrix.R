## Caching the Inverse of a Matrix

## makeCacheMatrix do
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Do invers of 'x', but previosly it checks if invers has been already done

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv))
        {
         message("getting cached data")
         return(inv)
        }
        mdata <- x$get()
        inv <- solve(mdata)
        x$setinv(inv)
        inv
}

##example 
# A = matrix(c(0,1,2,1), nrow=2, ncol=2, byrow=TRUE)
# cacheSolve(makeCacheMatrix(A))
