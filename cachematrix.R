## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function 
## This function receive one matrix and manipulate this matrix with 
##list because $ run with list
## create the m and x1. the m variable is will be the inverse matrix 
## and x1 my list with the matrix.
## create the function set. To fill the list and his inverse
## create get. to get the list with the matrix
## create setsolve. to set the inverse of the matrix
## create getsolve. to get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  m <-NULL
  x1<-list()
  x1[[1]] <- x
  set <- function(y) {
    x1<-list()
    x1[[1]] <<- y
    m[[1]]<<-solve(x[[1]])
  }
  get <- function() x1
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## Write a short comment describing this function
## this function receive of the list created with makeCacheMatrix() and
## check if this list have the inverse, if dont have create this inverse 
## and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m[[1]] <- solve(data[[1]])
  x$setsolve(m)
  m[[1]]
}

##test##
c<-matrix(55:58,2,2)
vec<-makeCacheMatrix(c)
vec1<-cacheSolve(vec)
vec1
vec$set(c)
vec$get()
vec$setsolve()
vec$getsolve()
