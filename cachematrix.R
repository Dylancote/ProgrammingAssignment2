##the makeCacheMatrix function contains 4 functions (set,get,setinv,getinv)
## get returns x, the matrix stored in the main function

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                m<-NULL
                set<-function(y){## changes the matrix stored in main fxn
         ## the 1st "<<-" substitutes matrix y w x in the main fxn
         ## rather than the set fxn. The second "<<-" restores the value of inverse m
                        x<<-y
                        m<<-NULL
                }
                get <-function() x ##  returns x, the matrix stored in the main fxn
                setinv<-function(solve) m<<-solve ##stores value of solve
                getinv<-function() m ## returns stored value m
                ##use list()  so that when  a matrix is inputted to makeCacheMatrix
                ## all 4 functions will be applied
                list(set = set, get = get, 
                     setinv=setinv,
                     getinv=getinv)
}

##CacheSolve determines if  the inverted matrix has been cached. if So, it returns
## the cached value. if not, it computes the inverse, caches it, and returns it.


cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {  ##verifies if m value exists (is cached).
                message("getting cached data") ## if it does it prints
                return(m) ##and returns the cached value of m
        }               ##if it isn't already cached:
        data <- x$get() ## it store the matrix  
        m <- solve(data, ...) ## and inverse the matrix
        x$setinv(m) ## will store the inverted matrix
        m ## will return the inverted matrix
}

