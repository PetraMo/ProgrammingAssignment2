### Assignment: Caching the Inverse of a Matrix by using lexical scoping


##Computing the inverse of a matrix is costly, 
##this pair of functions caches the inverse of a matrix so that the inverse calculation 
##does not have to be performed for an already calculated inverse


##Create a matrix x that can cache its inverse
##assume that x is square and invertible
##The 'makeCacheMatrix' function returns a LIST containing 4 functions to
##  set the matrix x
##  get the matrix x
##  set the inverse of the matrix x
##  get the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL             #set inverse for matrix x to null
        set <- function(y) {
                x <<- y
                invx <<- NULL    
        }
        get <- function() x
        setinverse <- function(solve) invx <<- solve
        getinverse <- function() invx
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



##Retrieve the matrix created with `makeCacheMatrix', if the matrix has been cached,
##retrieve cached inverse of the matrix
##if not, calculate the inverse of the matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', if it is available in list x (item x$getinverse() in list)
        invx <- x$getinverse()
        if(!is.null(invx)) {                  #if the inverse is found(not is null), return cached inverse 
                message("getting cached data")
                return(invx)
        }
        data <- x$get()                       # calculate the inverse if it was not yet cached
        invx <- solve(data, ...)
        x$setinverse(invx)
        invx
}
        


