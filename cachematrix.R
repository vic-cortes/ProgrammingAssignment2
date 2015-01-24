## This functions compute the inverse of a matrix. If the inverse matrix was already compute the function 
## will return the cached matrix in order to avoid computing the same inverse again.  

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(dat = matrix()) {
 M <- NULL
    datOld <- dat
    set <- function(y){ # This function sets the value of the "new matrix" to be computed
          datOld <<- dat
          dat <<- y
          M <<- NULL      
    }
    comp <- function() identical(dat, datOld) #Compare both matrices
    get <- function() datOld <<- dat
    setSolve <- function(solve) M <<- solve
    getSolve <- function()M
    list( set = set, comp = comp,          
          get = get, setSolve = setSolve,
          getSolve = getSolve)  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	L <- x$comp()
    	F <- x$getSolve()
    	if((L == TRUE) & (!is.null(F))){         #If the matrix didn't change and the getSolve is "not NULL"  
          M <- x$getSolve()                    #then will return the cache Matrix, otherwise it will compute
          message("Getting the Inverse Matrix")#the new inverse.
          return(M) 
    	}
    	data <- x$get()
    	M <- solve(data, ...)
    	x$setSolve(M)
    	M  

}
