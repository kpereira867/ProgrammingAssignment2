#makeCacheMatrix creates a matrix object, using a square invertible matrix as input, whose inverse can be later cached once
#called upon by another function (e.g. cacheSolve). If the inverse of one of the matrix values has not been solved, the value
#will be NULL.

makeCacheMatrix <- function (x = matrix ()) {
  matrix_inv = NULL
  set = function (y){
    x <<- y
    matrix_inv <<- NULL
  }
  
  get = function ()x
  setinv = function (inverse) matrix_inv <<- inverse
  getinv = function () matrix_inv
  list (set = set,  get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve evaluates a inversible square matrix by going through each of the values within that matrix, if the inverse
# has already been solved for a particular value, the function will simply return the value of the inverse. If the 
# inverse has not been taken (value of cached matrix = NULL) then the function will solve the inverse for the value and then return the inverse value.

cacheSolve <- function(x, ...) {
  matrix_inv = x["getinv()"]
  
  if(!is.null(matrix_inv)){
    message ("getting cached data")
    return(matrix_inv)
  }

  
  x$setinv(matrix_inv)
  retrun(matrix_inv)
}
git push origin master
