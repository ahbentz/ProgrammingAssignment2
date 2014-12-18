################################################################################
## These functions set up, solve and cache the inverse of a matrix into memory##
## so they can be called on when needed.                                      ##
################################################################################

#########################  makeCacheMatrix  ####################################
## The 'makeCacheMatrix' function takes a matarix as an argument (x) and      ##
## assigns it to an object inside the function environemnt. The inverse object##
## is set to null upon the intitializing of the function and upon calling the ##
##"set" function within the function environment (ultimately x$set).          ##
################################################################################

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL #Set inverse to null for later use and as a reset upon 
                  #calling function for the second or more time.
      set <- function(y) {
            #Assigns the matrix to an object (set) within the 'makeCacheMatrix' 
            #function environment - the '<<-' allows this to be in the 
            #'makeCacheMatrix' environment and not the 'set' function environment
            x <<- y 
            inv <<- NULL #Reset (see above)
      }
      #Returns the original matrix that was set
      get <- function() x 
      #sets the inverse; for use in 2nd function
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv #Returns the inverse; for use in second function
      #Assigns each of these functions to a list so they can be called with 'x$function'
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


#########################  cacheSolve  #########################################
## The 'cacheSolve' function takes a matarix as an argument (x)               ##
## x is an object with the functions as set through 'makeCacheMatrix'         ##
## 'cacheSolve' returns either                                                ##
## a) the cached inverse matrix or b) solves the matrix and returns that value##
################################################################################

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse() # assigns 'inv' to be the cached value
      # checks to see if cached value exists (!is.null(inv) = TRUE)
      if(!is.null(inv)) {
            message("getting cached matrix")
            return(inv)
      }
      # else
      # sets 'matrix' object to the argument matrix
      matrix <- x$get()
      # Solves the matrix
      inv <- solve(matrix)
      # Sets the newly solved matrix to the x object
      x$setinverse(inv)
      # Returns the inverse
      inv
}
