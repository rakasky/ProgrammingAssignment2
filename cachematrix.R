## This first function creates a list which stores functions
## which helps, in storing a matrix data and its inverse

## This function creates a list of functions to set a matrix data,
## get a matrix data, set inverse of matrix and get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## This function checks the inverse of x exists in cache
## if it exists in cache, it returns the cache otherwise it 
## calculates the inverse and sets it in the cache

cacheSolve <- function(x, ...){
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
        
}

### how to use these functions
## 1. x < - rbind(c(1,2),c(3,4)) # create a simple matrix
## 2. matrix_list <- makeCacheMatrix(x) # use the first function to create a list of functions
## 3. cacheSolve(matrix_list) # check the cache or calculate inverse


