## Caching the Inverse of a Matrix Assignment


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    ## Set m as Inverse
    m <- NULL
    
    ## Set Matrix function
    set <- function(y)
    {
        x<<-y
        m<<-NULL
    }
    
    ## This will call the Matrix Function
    get<-function() x
    
    ## Method to set the inverse of the matrix
    setinverse<-function(inverse) m<<-inverse
    
    ## Calling the inverse matrix function
    getinverse<-function() m
    
    ## Shows all functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Returns the inverse of 'x'
    m <- x$getinverse()
    
    ## returns the inverse if it is already set
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    
    ## Gets the matrix
    data<-x$get()
    
    ## Calculate the inverse 
    m<-solve(data,...)
    
    ## Set the inverse 
    x$setinverse(m)
    
    ## Return the final matrix
    m
}
