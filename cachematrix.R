## Matrix inversion is usually a costly computation and hence when repeated 
## computation of the matrix inverse is needed, it is preferable to cache the
## inverse, and return the cached version where possible instead of re-computing 
## every time. This program has two functions which enable us to do this.
## 
## The function `makeCacheMatrix` takes a matrix as input, and returns a special
## "matrix", which is essentially a list containing following functions, which
## allows us to set and retrieve the matrix and its inverse, and an additional
## function that tells us if the cache is empty(FALSE) or full(TRUE)
## 1. `set` - set the value of the matrix
## 2. `get` - get the value of the matrix
## 3. `setCache - stores the value of the matrix inverse into the cache
## 4. `getcache - gets the value of the matrix inverse from the cache
## 5. `getCacheStatus` - get current status of cache: TRUE=full; FALSE=empty

makeCacheMatrix <- function(x = matrix()) {
    
    # the inverse is cached in a matrix called cache. The logical variable 
    # 'cacheStatus states the current status of the cache. TRUE=full; FALSE=empty
    
    cache <- matrix()        # initialise the cache to an empty matrix
    cacheStatus<-FALSE       # initialise cacheStatus to FALSE to represent empty cache
    
    set <- function(y) {
        x <<- y
        cache <<- matrix()   # when the values of matrix are re-set, empty the cache
        cacheStatus<<-FALSE  # and set cacheStatus to empty(FALSE)
    }

    get <- function() x
    
    setCache <- function(inverse){
        cache <<- inverse    # store inverse in the cache
        cacheStatus<<-TRUE   # and set cacheStatus to full(TRUE)
    }
    
    getCache <- function() cache
    
    getCacheStatus <- function() cacheStatus
    
    list(set = set, get = get,
         setCache = setCache,
         getCache = getCache, getCacheStatus=getCacheStatus)
}


## The function `cacheSolve` calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of 
## the data and sets the value of the inverse in the cache

cacheSolve <- function(x, ...) {
    #if cache is full(TRUE), get inverse from cache, else calculate
    if(x$getCacheStatus()) { 
        message("getting cached data")
        return(x$getCache())
    }
    data <- x$get()
    inverse <- solve(data,...)
    x$setCache(inverse)    #once inverse is calculates, store it in cache for future
    inverse
}
