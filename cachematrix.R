## The following two functions will cache inverse of a matrix
## and retrieve the cached data if available; if no cached data,
## inverse of the matrix is computed and returned.

## This function creates a special "Matrix" object that can 
## cache its inverse.
## input: a matrix
## output: a list of 4 functions

makeCacheMatrix <- function(x = matrix()) {
	inverseOfM <- NULL;

	set <- function(y){
		x <<- y;
		inverseOfM <<- NULL;
	}

	get <- function() x;

	setInverse <- function(i) inverseOfM <<- i

	getInverse <- function() inverseOfM;

	list(set=set, get=get,
		 setInverse=setInverse,
		 getInverse=getInverse);	
}


## This function returns the inverse for the matrix created by 
## the above function - makeCacheMatrix() either by computing 
## the inverse or retrieving it if it has been cached.
## input: a special "Matrix" object created by makeCacheMatrix
## output: inverse of the input matrix object

cacheSolve <- function(x, ...) {
        
	inverseInCache <- x$getInverse();
	
	if(!is.null(inverseInCache)) {
		message("getting cached data");
		return(inverseInCache);
	}

	matrixInCache <- x$get();
	inverseInCache <- solve(matrixInCache, ...);
	x$setInverse(inverseInCache);
	inverseInCache;
}

