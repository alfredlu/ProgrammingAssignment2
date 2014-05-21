## This script is for caching potentially time-consuming computation
## in calculating inverse matrix

## Versions
# 1.0.0, 2014-05-21, AlfredWJLu@gmail.com
#   Initial one

## This function serves as the construction of a cache matrix
## if a invoking happend without any argument, a empty matrix
## is stored...

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        ## This assignment operator will change the 
        ## variable in parent enviroment (calling env)
        ## which means a member in the instance of makeCacheMatrix
        ## instead of the one in the definition.
        x <<- y
        m <<- NULL # since the x is changed, we have to recalculate inv
    }
    get <- function() x
    setInv <- function(invM) m <<- invM
    getInv <- function() m
    
    ## This list encapsulate the functions
    ## like public member in OO, and since
    ## this list is returned by makeCacheMatrix
    ## it could serve as a instance of makeCacheMatrix...
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## This function calculate the inverse matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    if(sum(is.na(data))) {
        stop("No original matrix, call set or reconstruct it...");
    }
    ## Now we calculate inv matrix and feed it to m.
    ## QR decomposition is a good choice. 
    m <- qr.solve(data)
    x$setInv(m)
    m
}

## Examples for using these two function

# mins <- makeCacheMatrix();
# mins$set(matrix(c(1,2,3,4),ncol=2,nrow=2));
# minv <- cacheSolve(mins);minv
# minv <- cacheSolve(mins);minv
# 
# mins$set(matrix(c(5,6,7,8),ncol=2,nrow=2));
# minv <- cacheSolve(mins);minv
# minv <- cacheSolve(mins);minv
# 
# mins <- makeCacheMatrix(matrix(c(7,8,9,10),ncol=2,nrow=2));
# minv <- cacheSolve(mins);minv
# minv <- cacheSolve(mins);minv