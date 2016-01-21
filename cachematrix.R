## Functions to cache the inverse of a matrix


## makeCacheMatrix() creates a special "matric" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        
        list(get=get, set=set, getinv=getinv, setinv=setinv)
}


## cacheSolve() computes the inverse of a given matrix. If the
## inverse has already been calculated, then the cacheSolve()
## retrieves the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached inverse")
                return(inv)
        }
        
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv
}
