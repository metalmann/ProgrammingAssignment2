## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(param1 = matrix()) {
   #Assigns a variable holder the NULL value
  holder <- NULL
  
   # creates a function "set" that takes in mat1 object as parameter 
  set <- function(mat1) {
    
    # This function sets the "permanent" value of parameter passed 
    # of parent function "makeCaheMatrix" to object passed in "set"
  
    param1 <<- mat1
    
    # This function sets the "permanent" value of holder variable 
    # declared in "makeCaheMatrix" to NULL
    
    holder <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
