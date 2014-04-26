#This script is designed to calculate the inverse of a target matrix. As inverting matrices can be computationally 
#intensive these functions will only solve for the inverse matrix if it has not been cached into memory.

# This function will generated a cached matrix, unless it is currently cached in memory
makeCacheMatrix <- function(x = matrix()) {
    #Generate the variable for the stored matrix
    Matrix <- NULL
    
    #This will set the stored matrix to the input values
    set <- function(y) {
      x <<- y
      Matrix <<- NULL
    }
    
    #This retrieve the matrix submitted to the function
    get <- function() x
    
    #This will set the matrix variable to the inverse
    SetMatrix <- function(solve) Matrix <<- solve
    
    #This will retrieve the inverse matrix
    GetInverse <- function() Matrix
    
    #This collects the functions above into a list
    list(set = set, get = get,
         SetMatrix = SetMatrix,
         GetInverse = GetInverse)
}

# Calling this function will give the inverse of the submitted matrix by calling the makeCacheMatrix function.
cacheSolve <- function(x, ...) {
    #Return a matrix that is the inverse of 'x'
    Matrix <- x$GetInverse()
    
    #If there is already a cached inverted matrix then retrieve it
    if(!is.null(Matrix)){
      message("Getting Cached Data")
      return(Matrix)
    }
    
    #If there is no stored value then the remaining portion of this function 
    #calculate the inverted matrix, return it and then store it in the cache
    data <- x$get()
    Matrix <- solve(data, ...)
    x$SetMatrix(Matrix)
    Matrix
}
#############################################################
#This section contains some test code
ExampleMatrix <- matrix(5:8,2)
#     [,1] [,2]
#[1,]    5    7
#[2,]    6    8

Cached_Matrix <- makeCacheMatrix(ExampleMatrix)

cacheSolve(Cached_Matrix)
#     [,1] [,2]
#[1,]   -4  3.5
#[2,]    3 -2.5

cacheSolve(Cached_Matrix)
#Getting Cached Data
#     [,1] [,2]
#[1,]   -4  3.5
#[2,]    3 -2.5
