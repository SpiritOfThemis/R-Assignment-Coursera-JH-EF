
makeCacheMatrix = function(m = matrix()) {
  Inv = NULL
  get = function() m #get matrix
  set <- function(m2){ #set matrix, reset inverse
    m <<- m2
    Inv <<- NULL
  }  
  
  getinv <- function() Inv
  
  # set the inverse of data matrix to inverse
  setinv <- function(inverse) Inv <<- inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve = function(m, ...) {
  Inv <- m$getinv() #get inverse
  if( !is.null(Inv) ){ #return inverse if inverse exists
    return(Inv)
  }
  matrix2 <- m$get() #if it doesn't have the inverse
  Inv = solve(matrix2)
  m$setinv(Inv)
  Inv
}
