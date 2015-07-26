## The makeCacheMatrix function creates a special matrix object, of which the cacheSolve 
## function calculates the inverse of.
## If the matrix inverse has already been calculated, it will then find it in the cache 
## and return it, and not recalculate it.  
## Otherwise, it calculates the inverse of the matrix and sets the value of the matrix 
## inverse in the cache via the setinverse function.


## The first function, makeCacheMatrix creates a special "matrix", which is really a list 
## containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_x <<- inverse
        getinverse <- function() inv_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function, cacheSolve returns the inverse of a matrix that was created by the 
## makeCacheMatrix function.
## If the matrix inverse has already been cached, then cacheSolve retrieves it. 
## Otherwise, it calculates the inverse of the matrix, sets it in the cache, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse()
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        } 
        data <- x$get()
        inv_x <- solve(data)
        x$setinverse(inv_x)
        return(inv_x)
}
