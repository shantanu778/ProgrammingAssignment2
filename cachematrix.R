## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	  set <- function(y) {
		x <<- y
		m <<- NULL
	  }
	  get <- function() x
	  setinverseMat <- function(inverseMat) m <<- inverseMat
	  getinverseMat <- function() m
	  list(set = set, get = get,
		   setinverseMat = setinverseMat,
		   getinverseMat = getinverseMat)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	m <- x$getinverseMat()
	  if(!is.null(m)) {
		message("getting cached data")
		return(m)
	  }
	  data <- x$get()
	  tryCatch({
		if(class(data) == 'matrix' & det(data) != 0){
		  m <- solve(data)
		}else{
		  message("Only non-singular matrices have an inverse")
		  
		}
	  },
		error = function(e){
		  message("x must be a Square Matrix")
		  
	  })
	  x$setinverseMat(m)
	  m
        ## Return a matrix that is the inverse of 'x'
}

