##Essentially, an edit of the example problem, changing key variables (mean -> inv, 
## vector -> matrix, etc...)

## We're trying to find the inverse of a cached matrix. The first chunk will be 
## making the matrix, essentially. The second chunk will be to calculate the
## inverse of said matrix. 


makeCacheMatrix <- function(x = matrix()){
 ## We start off making the matrix, and choosing what's going to go inside 
  i <-NULL
  set <-function(y){
    x<<- y
    i<<- NULL
  }
  ## First defined function: defines x as a matrix and other factors that will be used later
  get <-function()x
  setinv <-function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This sets the basic variables (x, i, y, get, etc...) Naming the 
##variables lets me go back and know that I'm looking for the inverse



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    ## ! makes an inverse of i, which is the our the list that we want an inverse of
    message("getting cached data")
    ## waiting message
    return(i)
  }
  data <- x$get()
  i <- inv(data, ...)
  x$setinv(i)
  i
}
