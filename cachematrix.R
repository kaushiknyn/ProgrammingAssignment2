## Put comments here that give an overall description of what your
## functions do:
## 1. The makeCacheMatrix creates a matrix object that can cache its inverse
## 2. The cacheSolveMatrix returns the inverse of the matrix retuned by the makeCacheMatrix above.
## If the inverse of the matrix has already been calculated and the matrix is not changed,
## then the cacheSolve should retireve the inverse from the cache

## Write a short comment describing this function:

## The makeCacheMatrix has four separate functions
## (get, set, getInverse and setInverse), that store the inverse of 
## the matrix calculated using the cacheSolve function to an object in the
## global environment. So, if the function is assigned to an object in the global environment,
## that object assumes the ability to execute all the funtions defined in the makeCacheMatrix
## function

makeCacheMatrix <- function(x = matrix()){
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get, 
             setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## The cacheSolve matrix starts off by calculating the inverse of a matrix.
## the if loop with in the function evalulates to see if the inveser of the
## matrix in the argument has already been cached. If it has it retrieves the
## inverse value is stored using the cache function and returns it without
## having to calculate the inverse all over again

cacheSolve <- function(x, ...){
        i <- x$getInverse()
        if(!is.null(i)){
                message("getting cached inverse matrix data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setInverse(i)
        i
}
