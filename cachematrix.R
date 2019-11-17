## Put comments here that give an overall description of what your
## functions do

## This function is done to:
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(InverseMatrix) m <<- InverseMatrix
  getInverseMatrix <- function() m
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## This is the cachesolve function. We look if the inverse of the matrix has already been computed. If yes, we retrieve the value otherwise we compute it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverseMatrix(m)
  m
}

#testing: 
#my_matrix <- matrix(5:8, nrow = 2, ncol = 2)
#x <- makeCacheMatrix(my_matrix)
#cacheSolve(x)

