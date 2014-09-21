## This code is for caching the inverse of a matrix. There are 2 functions used :-

## (1) makeCacheMatrix function is used for getting and setting the matrix and its inverse

## (2) cacheSolve function is used to retrieve the value of the inverse from the  cache
##     if it exsists there otherwise set the value of the matrix and its inverse in the
##     cache using makeCacheMatrix function.



## makeCacheMatrix function has  functions for getting and setting the value of the matrix
## and its inverse. Then alist of these function objects is created which will be used to 
## access them from cacheSolve function.


makeCacheMatrix <- function(maty=matrix()) {
    inv <- NULL
    set <- function(maty) {
        matx <<- maty
        inv <<- NULL
    }
    get <- function() matx
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve function returns inverse from the cache if the inverse exsists there 
## otherwise it will use the list of functions created in makeCacheMatrix function
## to set the value of matrix and its inverse in the cache


cacheSolve <- function(maty=matrix(),func) {
    inv <- func$getinv()
    if(!is.null(inv)) {
        message("getting cached Inverse")
        return(inv)
    }
    
    func$set(maty)
    data <- func$get()
    inv <- solve(data)
    func$setinv(inv)
    inv
}


func <- makeCacheMatrix(maty)
maty <- matrix(data = c(1,2,3,0,1,4,5,6,0),nrow = 3,ncol = 3,byrow = TRUE)
slv <- cacheSolve(maty,func)
slv

