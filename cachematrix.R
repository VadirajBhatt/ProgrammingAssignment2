## Below are two functions that are used to create a
## special object that stores a matrix and caches its inverse.

## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to
##
## 1.  set the matrix
## 2.  get the matrix
## 3.  set the matrix inverse
## 4.  get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() { x }
        setinverse <- function(inverse) { m <<- inverse }
        getinverse <- function() { m }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets inverse in the cache via the `setinverse`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        
        m <- solve(data,...)
        x$setinverse(m)
        m
}
