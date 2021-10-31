# Program purpose - return cached matrix after first operation to save time 
# mainly use <<- operator which can be used to assign a value to an object in an environment that is different from the current environment.
# Use solve function to invert matrix and use <<- operator to save into cache of an environment
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) 
{
  # makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
  myInverseMatrix <- NULL #reset and initialize the variable and ensure it's clean and has null value, no cached value
  
  set <- function(y) 
  {
    # <<- operator which can be used to assign a value to an object in an environment that is different from the current environment.
    x <<- y #matrix assigned from the first environment
    
    #reset myInverseMatrix
    myInverseMatrix <<- NULL #reset and initialize the variable and ensure it's clean and has null value, no cached value
  }
  
  get <- function() 
  {
    #for extra information on the matrix - must be invertible matrix
    message("getting matrix (not cached)")
    cat("number of rows: ", nrow(x), "\n")
    cat("number of columns: ", ncol(x), "\n")
    x
  }
  
  set_myInverseMatrix <- function(inverted_Matrix) 
  {
    # <<- operator which can be used to assign a value to an object in an environment that is different from the current environment.
    myInverseMatrix <<- inverted_Matrix
  }
  
  
  get_myInverseMatrix <- function() 
  {
    #for debugging to ensure inverted matrix has the same dimension as the original
    message("getting inverse matrix (not cached)")
    cat("number of rows: ", nrow(x), "\n")
    cat("number of columns: ", ncol(x), "\n")
    myInverseMatrix
  }
  
  #list coercion
  list(set = set, get = get, set_myInverseMatrix = set_myInverseMatrix, get_myInverseMatrix = get_myInverseMatrix)
}

cacheSolve <- function(x, ...) 
{
  #Return a matrix that is the inverse of 'x'
  #cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  myInverseMatrix <- x$get_myInverseMatrix()
  
  if(!is.null(myInverseMatrix)) 
  {
    message("getting cached matrix")
    
    #matrix cached from the last <<- operator that saved on the environment
    return(myInverseMatrix)
  }
  
  myMatrix <- x$get()
  
  #using the solve function to invert the matrix
  myInverseMatrix <- solve(myMatrix, ...)
  
  
  x$set_myInverseMatrix(myInverseMatrix)
  
  #return the matrix that inverted
  myInverseMatrix
}
