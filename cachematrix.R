
## Put comments here that give an overall description of what your

## functions do
 
## Below are two functions that are used to create a special object that stores an invertible matrix and cache's its inverse.

 

## Write a short comment describing this function 
## makeCacheMatrix creates a special "matirx", which is really a list containing a function to 
##  1.set the value of the matrix
##  2.get the value of the matrix
##  3.set the inverse of the matrix
##  4.get the inverse of the matrix

 

makeCacheMatrix <- function(x = matrix()) { 
 
        invmat <- NULL
        set <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setinv <- function(solve) invmat <<- solve
        getinv <- function() invmat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
 
 
 

 

## Write a short comment describing this function

## This function calculates the inverse of the special "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data which is a matrix and sets the value of the inverse in the cache via the setinv function.
 

cacheSolve <- function(x, ...) { 

       ## Return a matrix that is the inverse of 'x'

        invmat <- x$getinv()

        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data, ...)
        x$setinv(invmat)
        invmat
}