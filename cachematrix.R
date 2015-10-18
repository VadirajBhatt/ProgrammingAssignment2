## Coursera - R Programming - Programming Assignment 2: Lexical Scoping
## Date: 19-Oct-2015

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
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() { x }
        setinverse <- function(inverse) { i <<- inverse }
        getinverse <- function() { i }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets inverse in the cache via the `setinverse`
## function. We are using R inbuilt function "solve" for inverse computation.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        
        i <- solve(data,...)
        x$setinverse(i)
        i
}
