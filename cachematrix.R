## makeCacheMatrix creates a special "matrix" object that can cache its 
## inverse of a square matrix with the solve function
## makeCacheMatrix function create list of functions

# set the matrix
# get the matrix
# set the inverse of the square matrix
# get the inverse of the square matrix

makeCacheMatrix <- function(x = matrix()) {

      # define the set function and use << to assign value to an object in
      # different environment than the current one

      i <- NULL
      set <- function(y) {
            x <<- y # substitutes the vector x with y in makeCacheMatrix 
            i <<- NULL # old value is no needed anymore
      }
      
      # define get, setinverse, getinverse
      # these get values in these lines
      get <- function() x
      setinverse <- function(inv) i <<- inv
      getinverse <- function() i
      
      # list containing functions to set and get the matrix and it's inverse
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve: return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

      # check if the cache is empty, if not, use the cache data and return it
      # return means that function will ended here
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }

      ## if the cache is empty, use the solve function to make inverse of square matrix
      # get the data
      data <- x$get()
      
      # use the solve function to get inverse of the square matrix, 
      # put it in the cache and print the value
      i <- solve(data)
      x$setinverse(i)
      i
}