### Assignment: Caching the Inverse of a Matrix
## Put comments here that give an overall description of what your
## functions do

##Computing the inverse of a matrix is costly, 
##this pair of functions caches the inverse of a matrix so that the inverse calculation 
##does not have to be performed for an already calculated inverse

## Write a short comment describing this function
##Create a matrix x that can cache its inverse
##assume that x is square and invertible
##The 'makeCacheMatrix' function returns a LIST containing 4 functions to
##  set the matrix x
##  get the matrix x
##  set the inverse of the matrix x
##  get the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) invx <<- solve
        getinverse <- function() invx
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
##Retrieve the matrix created with `makeCacheMatrix', if the original matrix remained same and has been cached,
##retrieve cached inverse of the matrix
##if not, calculate the inverse of the matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invx <- x$getinverse()
        if(!is.null(invx)) {
                message("getting cached data")
                return(invx)
        }
        data <- x$get()
        invx <- solve(data, ...)
        x$setinverse(invx)
        invx
}
        
k<-rbind(c(2,7),c(2,8))
mymatrix<-makeCacheMatrix(k)
print(mymatrix)
mycachedinverse<-cacheSolve(mymatrix)
print(mycachedinverse)
mymatrix2<-makeCacheMatrix(k)
print(mymatrix2)
print(mycachedinverse==cacheSolve(mymatrix)) ##here is the first time that is shows message("getting cached data")
mycachedmatrix2<-cacheSolve(mymatrix)##here is the second time that is shows message("getting cached data")
