## makeCacheMatrix will create a list of 4 functions that with store 
## the matrix (set function). If we apply the second function 
## to the outcome of the 1st then it will store also the inverse in the cache
## (setinverse). If we want to recover one of them we use the functions
## get or getinverse, respectively.

## The function cacheSolve first check if there is already a calculated 
## inverse matrix, if there is a value of inv, it gets it, otherwise it 
## creates de novo the inverse matrix and stores it in the previous list 
## (setinverse function).

#####

# function that creates a list of 4 functions that will be used to store and 
# recover the matrix and the inverse matrix respectively.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL               # reset the variable inv
  # We set up the 4 functions: set, get, setinverse and getinverse
  set<- function(y) {
    x <<- y     
    inv <<- NULL 
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list (set=set, get=get, # create a list of 4 different functions
        setinverse=setinverse,
        getinverse=getinverse)
}


# This function will check first if there is an inverse matrix stored 
# in the list by calling x$getinverse and !is.null() function, consecutively.
# If inv= NULL it jumps to the else statement and creates de inverse matrix
# using the function solve(). Then it outputs and stores the result in x$setinverse(). 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)           # returns the stored inverse matrix
  } else {
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)       # stores de inverse matrix
  inv                     # returns the inverse matrix
  }
}
