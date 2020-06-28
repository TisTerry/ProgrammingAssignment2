## Cache inverse of a matrix

## create a vector of functions

makeCacheMatrix <- function(x = matrix()) {
    m<- NULL
    set<-function(y){
        x<<-y
        m<<-NULL 
    }
    get<-function() x
    setinverse<-function(inverse) m<<-inverse
    getinverse<-function() m
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}


## get the cached inverse or calculate the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getinverse()
    if(!is.null(m)){
        message("getting cashed data")
        return(m)
    }
    else{
        data<-x$get()
        m<-solve(data)
        x$setinverse(m)
        m
    }
}
