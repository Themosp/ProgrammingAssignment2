## This function computes the inverse of a matrix and caches it,
## so that it does not have to be computed if it has not changed. 

## The function creates a list of functions to be used as variables in another function
## The function of lists contains the following functions:
## set, which assigns the value of a matrix
## get, which retrieves the value of the matrix
## setInverse, which stores the value of the inverse matrix
## getIverse, which retrieves the value of the inverse matrix



makeCacheMatrix <- function(x = matrix()) {
  ## First, initialize the value of the inverse matrix
  invmtrx <- NULL
  
  set <- function(y) {
    ## Assign a values to variables to be reatined in an environment 
    ## that is different from the current environment (cache)
    x <<- y
    invmtrx <<- NULL
  }
  
  ## Retrieve the value of the original matrix
  get <- function() x
  
  ## Store the inverse of the matrix
  set_inv <- function(inv) invmtrx <<- inv
  
  ## Retrieve the inverse of the matrix
  get_inv <- function() invmtrx
  
  ## Return the list of the functions set, get, setInverse, getInverse
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## This function returns the inverse of the matrix 
## provided by the makeCacheMatrix function.
## If the inverse matrix has already been calculated, the value is retrieved from cache,
## else, if a cached matrix does not exist a new inverse matrix is calculated


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Retrieve the inverse of the matrix
  invmtrx <- x$get_inv()
  
  ## If the inverse exists (is not NULL) it means that it has already been cached
  if(!is.null(invmtrx)) {
    message("getting cached data")
    
    #Return the cached value and exit the function
    return(invmtrx)
  }
  
  ## If the inverse is NULL, get the original matrix
  data <- x$get()
  
  ## Calculate the inverse
  invmtrx <- solve(data, ...)
  
  ## Set the inverse of the matrix and return it
  x$set_inv(invmtrx)
  invmtrx
}
