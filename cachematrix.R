## The following two functions: 

## 1.) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## 2.) cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

## 1. makeCacheMatrix function, creates an object of the type list.
## It creates functions (or objects) used by cacheSolve() to get values 
#  for x, or for i (inverse) and for setting the inverse of a matrix.  

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL             # i stores the inverse of the matrix
        set <- function(y) {  # resets to NULL every time the function is called
                x <<- y
                i <<- NULL
        }
        get <- function() x   # fn. returns the value of the matrix
        
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## The second function, When called, it will see if the inverse of the matrix 
## has been stored.  
## If not, it will calculate the inverse of the matrix, store it and then return it. 
## If the inverse for this object has been calculated and stored earlier, 
## it will fetch the inverse and return it.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}