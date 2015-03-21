## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Definition of function named "makeCacheMatrix" with 
# Argument "x" with empty matrix as default
makeCacheMatrix <- function(x = matrix()) {# Definition of function starts
# Here We should organize the cache that consists of the two matrices:
# the initial matrix "x" is already got as argument and the inverse matrix "s"
  s <- NULL #Initially We have no solution

# Then We define the "set" function inside the function "makeCacheMatrix". This 
# function allows us to change the initial matrix in the already created cache
  set <- function(y) {
    x <<- y # Set initial matrix "x" in the parent environment
    s <<- NULL # When we just set initial matrix, we have no solution in cache
  }

# This function returns us initial matrix from cache
get <- function() x

# This function takes the inverse matrix inward and assigns its value
# from inside to "s" in the parent environment to store like in cache
  setsolve <- function(solve) s <<- solve

# This function returns us inverse matrix from cache
getsolve <- function() s

# Here the list of functions is returning
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  s <- x$getsolve() #Searching solution in the cache
  
  if(!is.null(s)) { #If there is solution in the cache
    message("getting cached data")
    return(s) #Ending the function using cache data
  }
  data <- x$get() #If there is no solution in the cache
  s <- solve(data, ...) #Make solution
  x$setsolve(s) #Set cache data
  s #Returning solution
}
