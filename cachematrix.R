## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will look for global environment 
## if the matrix is unchanged and was calculated the inverse before
## then it can be get() from the environment
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL #inversed matrix
        set <- function(y) { #set the input matrix equal to cached matrix(env)
                x <<- y
                i <<- NULL
        }
        get <- function() x # get the same matrix in the env. 
        setinverse <- function(inverse) i <<- inverse # inverse from env. 
        getinverse <- function() i #return the inverse 
        
        list(set=set, get=get, #return a list of 2 pairs get(), set()  for original
             setinverse=setinverse, # and for the inverse
             getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("the inverse of same matrix is already cached")
                message("result is")
                return(i)
        }
        
        data <- x$get() #no cache, load the matrix to compute
        i <- solve(data,...) #do the inversion by solve()
        x$setinverse(i) #assign i to x
        i #return the inversed matrix
}
