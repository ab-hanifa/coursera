makeCacheMatrix <- function(x = matrix()) { # Define the argument as mode of matrix
  inv <- NULL                              # initial inverse as a null and will store the inverse here 
  set <- function(y) {           # defining the set function to assign a new value 
    x <<- y                     # value of matrix in parent env
    inv <<- NULL               # setting as null if there is a new matrix
  }
  get <- function() x        # to get the matrix 
  setinverse <- function(inverse) inv <<- inverse   # define value of inv to parent env
  getinverse <- function() inv                     # get the the inv from parent env 
  list(set = set, get = get,           # listing all the function
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheinverse <- function(x, ...) {      # this function computes the special matrix
  inv <- x$getinverse()            # get the inverse matrix from the parent env
  if(!is.null(inv)) {            # if the inv matrix is not null then it called from the cached data
    message("getting cached data")
    return(inv)
  }
  matrix_to_inverse <- x$get() # if inv is null then the get the matrix
  inv <- solve(matrix_to_inverse, ...)     # computes the inverse matrix
  x$setinverse(inv)                      # the inverse and finally returns it
  inv
}

my_Matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_Matrix$get()
