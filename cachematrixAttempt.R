## Using makeCacheMatrix and cacheSolve

## Creating the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    t<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function(){x}
    setinverse<-function(inverse) {t<<-inverse}
    getinverse<-function(){t}
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Caching the value of the inverse of the matrix

cacheSolve <- function(x, ...) {
       t<-x$getinverse()
       if(!is.null(t)){
          message("Getting cached data")
         return(t)
       }
       mat<-x$get()
       t<-solve(mat,...)
       x$setinverse(t)
       t
}
