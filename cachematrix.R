## This set of 2 functions store (cache) the inverse matrix so that
## the calculations need not be done repeatedly

## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
   # locally Assigns a variable m the NULL value
  m <- NULL
  
   # creates a function set that takes in undeclared y 
   # object as parameter 
  set <- function(y) {
    
    # This function globally assigns value of param y 
    # to param passed to "makeCacheMatrix" (x)
  
    x <<- y
    
    # This function sets globally the value of holder variable 
    # declared in "makeCaheMatrix" to NULL
    
    m <<- NULL
  }
  
  # creates an empty function "get" that takes in nothing as parameter 
  # and returns object "x" passes as param in makeCacheMatrix
  
  get <- function() x
  
  # creates an  function getInv that takes in nothing as parameter 
  # and returns object "m"
  
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, 
       get = get,
       setInv = setInv,
       getInv = getInv)

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x',
        ## x being the object that is returned by function
        ## makeCacheMatrix 
  
  m <- x$getInv()
  
   ## Check for the value of m that either holds NULL or the calculated value 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
