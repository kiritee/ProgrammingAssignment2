## Matrix inversion is usually a costly computation and hence when repeated 
## computation of the matrix inverse is needed, it is preferable to cache the
## inverse, and return the cached version where possible instead of re-computing 
## every time. This program has two functions which enable us to do this.
## 
## The function `makeCacheMatrix` takes a matrix as input, and returns a special
## "matrix", which is essentially a list containing following functions, 
## which allows us to set and retrieve the matrix and its inverse, and an additional
## function that tells us if the cache is empty(FALSE) or full(TRUE)
## 1. `set` - set the value of the matrix
## 2. `get` - get the value of the matrix
## 3. `setInverse - sets the value of the matrix inverse
## 4. `getInverse - gets the value of the matrix inverse
## 5. `getCacheStatus` - get current status of cache: TRUE=full; FALSE=empty

makeCacheMatrix <- function(x = matrix()) {
    inv <- matrix()
    cached<-FALSE #cache is empty in the beginning
    
    set <- function(y) {
        x <<- y
        inv <<- matrix()
        cached<<-FALSE #when the values of matrix are re-set, empty the cache
    }

    get <- function() x
    
    setInverse <- function(inverse){
        inv <<- inverse
        cached<<-TRUE #once the inverse is assigned, set cache to full
    }
    
    getInverse <- function() inv
    
    getCacheStatus <- function() cached
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse, getCacheStatus=getCacheStatus)
}


## The function `cacheSolve` calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of 
## the data and sets the value of the inverse in the cache

cacheSolve <- function(x, ...) {
    #if cache is full, get inverse from cache, else calculate
    if(x$getCacheStatus()) { 
        message("getting cached data")
        return(x$getInverse())
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setInverse(inv)
    inv
}
