makeCacheMatrix <- function(x = matrix())
{
  inverse <- NULL
  set <- function(n)
  {
    x <<- n
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <<- function() inverse
  list
  (
    set = set
    ,get = get
    ,setinverse = setinverse
    ,getinverse = getinverse
  )
}




cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) # checks if inverse already exists
  {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data,...)
  x$setinverse(inverse)
  inverse # returns calculated inverse
}
