## cachematrix contains two functions: makeCacheMatrix and cacheSolve. These functions work to
## make calculations, that can be time intensive, more concise by searching for a cached value.

## makeCacheMatrix takes in a matrix and returns a value and a list that can be called into 
## cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  In <- NULL
  set<- function(y){
    x<<-y
    In <<- NULL
  }
  get <- function() x
  setIn <- function(solve) In<<- solve
  getIn <- function() In
  list(set = set, get = get,
       setIn = setIn,
       getIn = getIn)
}


## cacheSolve looks for a cached value of the matrix inverse, and if one does not exist,
## the inverse is calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  In <- x$getIn()
  if(!is.null(In)){
    message("getting cached data")
    return(In)
  }
  data <- x$get()
  In <- solve(data, ...)
  x$setIn(In)
  In
  }
