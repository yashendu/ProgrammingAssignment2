#----------------Assignment: Caching the Inverse of a Matrix----------------------

# Assumptions: For this assignment, assume that the given matrix is always invertible.



#There are two parts of this problem

# 1. makeCacheMatrix: This function creates a "matrix" object that can cache its inverse.
# 
# 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#                 If the inverse has already been calculated (and the matrix has not changed), 
#                 then the cachesolve should retrieve the inverse from the cache.


#---------------------------------- Matrix (object) function with Cache (inverse) ---------------------------------------------


## Define Object function

makeCacheMatrix <- function(x = matrix()){
        
        invm <- NULL
        
        # Define get, set for Matrix and Inverse
        
        set <- function(y){
                
                x <<- y
                invm <<- NULL
        }
        
        get <- function() x
        
        setinvm <- function(inverse) invm <<- inverse
        
        getinvm <- function() invm
        
        list(set = set, get = get, setinvm = setinvm, getinvm = getinvm)
}


##--------------------------------- Inverse matrix retrieval/ calculation -----------------------------------------------------

cacheSolve <- function(x, ...){
        
        
        
        ##Check if Inverse Matrix exists (in cache)
        invm <- x$getinvm()
        
        if(!is.null(invm)){
                
                message("retrieving cache")
                return(invm)
        }
        
        mat <- x$get()
        
        ## Calculate inverse of mat
        
        invm <- solve(mat, ...)
        
        ## Save the invm value 
        x$setinvm(invm)
        
        ## Print the values
        invm
        
}