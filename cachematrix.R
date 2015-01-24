## Assignment 2

## This function creates a special "matrix" object that can cache its inverse.
## Pre-condition:- matrix supplied must always be invertible
makeCacheMatrix <- function(x = matrix()) {
         m <- NULL
         set <- function(y) 
             {
                 x <<- y
                 m <<- NULL #sets Null to the ext environment m
             }
         get <- function() x
         setinverse <- function(solve) m <<- solve # Inverses the matrix
         getinverse <- function() m
         list(set = set, get = get,
                         setsolve = setinverse,
                         getinverse = getinverse) #lists the environment
}

##  cacheSolve : This function computes the inverse of the special "matrix" returned by  makeCacheMatrix  above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then  cacheSolve  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getinverse()
         if(!is.null(m)) {
                 message("getting cached data") 
                 return(m) # retrieves the cache m
             }
         data <- x$get()
         m <- solve(data, ...) #calculates the inverse matrix and assign to m
         x$setinverse(m)
         m

}
## End
