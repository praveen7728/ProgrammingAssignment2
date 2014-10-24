
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  	a<-NULL
  	set<-function(y){
    	x<<-y
    	a<<-NULL
    
  }
  	get<-function() x
  	setinverse<-function(solve) a<<- solve
  	getinverse<-function() a
  	list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  # Check that whether or not inverse of matrix exists in cache or not
  	a<-x$getinverse()
  
	if(!is.null(a)){
    
    	return(a)
  }
  ## Return a matrix that is the inverse of 'x'
  	matrix<-x$get()
  	a<-solve(matrix, ...)
  	x$setinverse(a)
  	a
}
