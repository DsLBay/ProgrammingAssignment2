## Put comments here that give an overall description of what your
## functions do
## In this assignment, there are two functions, makeCacheMatrix and cacheSolve.
## With these two functions, we can compute the inverse of a square matrix and
## cache the result so that we can retrieve it rather than computing again 
## the next time we deal with the same matrix. 

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve)  s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
## Function makeCacheMatrix creates a special "matrix", which is really a list containing
## a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

## Function cacheSolve calculates the inverse of the special "matrix" created with
## the above function. It first checks to see if the inverse has already been
## calculated. If so, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the
## inverse in the cache via the setsolve function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
