## This is to calculate and cache the inverse of a given matrix

## Create a list that contains functions to get and sets the inverse

makeCacheMatrix <- function(x = matrix()) {
  my_inverse <- NULL
  
  set <- function(y){
    x <<- y
    my_inverse <<- NULL
  }
  
  get <- function() x
  
  set_inverse <- function(inverse) my_inverse <<- inverse
  
  get_inverse <- function() my_inverse
  
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## This function calculates the inverse of the input matrix 
## or return the value of the cached version if it was already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  current_inverse <- x$get_inverse()
  
  if(!is.null(current_inverse)){
    message("getting cached data")
    return(current_inverse)
  }
  
  data <- x$get()
  current_inverse <- solve(data)
  x$set_inverse(current_inverse)
  
  return(current_inverse)
}
