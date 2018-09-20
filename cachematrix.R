## makeCacheMatrix function helps in creating a cache for the matrix and also provides reverse of it
## cacheSolve function helps in polling for matrix which is cached and prints the reverse of it

## Function Name: makeCacheMatrix
#  Argument     : Matrix vector
#  Return       : List of functions to set/get matrix or get/set reverse of
#                 matrix provided as input
#  Functions    : solve function is used to reverse the matrix which is accessed by its get/set
#                 simple get/set functions are defined to cache the matrix

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   
   # Function call to set the value
   set <- function (y){
             x <<- y
             m <<- NULL
   }
   
   # Function call to get the value
   get <-function() x
   
   # Function call to set the reverse of the matrix
   setReverse <- function (solve) m <<- solve
   
   # Function call to get the reverse of the matrix
   getReverse <- function() m
   
   list (set = set, get = get, setReverse = setReverse, getReverse = getReverse)
}


## Function Name: cacheSolve
#  Argument     : Matrix vector
#  Return       : Reversed vector
#  Functions    : Access the matrix if it is cached, else access it from the matrix first time.
#                 This is followed by calling functions from makeCacheMatrix to reverse the 
#                 matrix and return it.
cacheSolve <- function(y, ...) {
        ## Return a matrix that is the inverse of 'x'
  # get the cached value
  m <- y$getReverse()
  
  # if a cached value exists return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- y$get()
  m <- solve(data)
  y$setReverse(m)
  
  # return the inverse
  m
}