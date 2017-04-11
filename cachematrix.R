## This is a function containing two subfunctions. The 1st one is called makeCacheMatrix.
## It basically is like makeVector in the example of the assignment, which creats a special
## matrix containing functions to: 1 set the value of the matrix 2 get the value of the matrix 
## 3 set the value of the inverse 4 get the value of the inverse. The 2nd one is called 
## cacheSolve. It is very much alike the cacheMean in the example. It caculates the inverse of 
## matrix defined in the fist function. If the inverse is already calculated, it gets the 
## inverse directly. Otherwise, it calculate the inverse, set the value of the inverse by 
## setinverse function within the 1st function.

## The fist function makeCacheMatrix first defines the value of the matrix. It is special 
## because it also contain functions. The default value of matrix is blank. m is set as NULL
## unless you really cacheSolve(x) using the cacheSolve function. If you already cacheSolve
## it, then next time you type yourmatrix$getinverse(), you can call it directly.

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
set<-function(y){
        x<<-y
        m<<-NULL
}
get<-function() x
setinverse<-function(solve) m<<-solve
getinverse<-function() m
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## The cacheSolve function is used to calculate the inverse of the matrix defined in the
## first function. If the inverse already been calculated before, this function will give
## message "getting cached data" and then give the value. Otherwise, it starts to calculate
## the inverse of the matrix using solve() function and return the calculated value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m<-x$getinverse()
   if(!is.null(m)){
           message("getting cached data")
           return(m)
   }
   data<-x$get()
   m<-solve(data,...)
   x$setinverse(m)
   m
           }
