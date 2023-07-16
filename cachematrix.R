# These functions are based on the Coursera Data Science: R Programming 
# Week 3-Assignment

# Assignment: Caching the inverse of a matrix object
#Part 1: Creating a function to generate a "matrix" object

#The makeCacheMatrix function creates a special "matrix" object that can cache its inverse
    
makeCacheMatrix <- function(x = matrix()) { # defining the argument of the function
    i <- NULL                             # define the inverse as NULL
    set <- function(y) {                    # programming a function to assign new matrix-value in other environment
        x <<- y                             
        i <<- NULL                        # if there is a new matrix, define inverse again to NULL
    }
    get <- function() x                     # defining a function to return value of the argument
    
    setinverse <- function(inverse) i <<- inverse  # setting value of the inverse in other environment
    getinverse <- function() i                     # getting the value of inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}
    
#Part2: Calculating the inverse of a matrix with a second function

#The cacheSolve function computes the inverse of the special "matrix" returned by makeCakeMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),it will retrieve the inverse from the cache
    
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("get stored data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
