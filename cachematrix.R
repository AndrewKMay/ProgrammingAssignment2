## A pair of functions that enable the caching of a matrix inverse (makeCacheMatrix),
## that can be calculated (and subsequently cahced) using the cacheSolve funciton

## makeCacheMatrix provides a list of functions (needed by cacheSolve) to store a 
## user-provided matrix and it's inverse. The function does not compute any values itself. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #creating a placeholder for a subsequent value
  set <- function(y) { #defines a new function that sets matrix x to y, and resets i to NULL
    x <<- y
    i <<- NULL
  }
  get <- function() x #returns the matrix x
  setinverse <- function(inverse) i <<- inverse #sets the matrix inverse (i) to 'inverse'
  getinverse <- function() i #returns the matrix inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) #returns the special vector - a list of the above functions
}


## cacheSolve accepts a makeCacheMatrix object and either returns a cached matrix inverse,
## or calculates the inverse for the matrix originally passed to makeCacheMatrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse() #using the getinverse function defined previously, set the result to i
  if(!is.null(i)) { #if i is NOT null, then return whatever cached value is stored already
    message("getting cached data")
    return(i)
  }
  data <- x$get() #if i is null, use the get function to set the original matrix x, to 'data'.
  i <- solve(data, ...) #use the solve function to find the inverse of matrix x, setting it to i
  x$setinverse(i) #input this inverse matrix into the makeCacheMatrix function setinverse
  i #return the calculated matrix inverse
}
