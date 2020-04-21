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


## Here I first check the cache if cache is empty. I check the class of the input whether it is a "Matrix" or not.
## Then, I also check the determine of that matrix. because if the determination of matrix is equal to 0 then we can't find the Inverse Matrix.
## I also keep that process into tryCatch() function if Given Matrix is not a Square then it will also raise an Error. So I put a check Here too.
## After finding the inverse Matrix I set it on Cache and then return the Inverse matrix value.
 
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

