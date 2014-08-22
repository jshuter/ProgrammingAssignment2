## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and their may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly 
## This Your assignment is to write a pair of functions that cache the inverse of a matrix.


## ------------------------------------------------------------------------
## makeCacheMatrix creates "a matrix object that can cache its inverse" 
## ------------------------------------------------------------------------

test <- function(){ 

        #------------------------
        # get the inverse 3 times
        #------------------------
        
        cache=makeCacheMatrix()
        
        m1=matrix(sample(1:25,25, replace=TRUE),5,5)
        print (m1)
        cache$set(m1)
        
        inv=cacheSolve(cache);
        print (inv); 
        
        inv=cacheSolve(cache);
        print (inv); 
        
        inv=cacheSolve(cache);
        print (inv); 
        
        # make a new matrix
        m99=matrix(sample(1:16,16, replace=TRUE),4,4)
        print (m99)
        cache$set(m99)
        
        # cache - matrix has changed ... 
        inv=cacheSolve(cache);
        print (inv); 
        
        # cache - matrix has changed ... 
        inv=cacheSolve(cache);
        print (inv); 
        
        #------------------------
        # inverse of the inverse
        #------------------------
        print('-------------------------------------------------- ...')
        print('Inverse of Inverse should revert to original Matrix...')
        m1=cacheSolve(cache)
        print (m1)
        cache$set(m1)
        
        inv=cacheSolve(cache);
        print (inv);         
}


makeCacheMatrix <- function(passed_mx = matrix()) {

        this_inv <- NULL
        
        # setter / create this 'object', save the matrix 
        set <- function(inner_mx) {
                print('setting ...')
                passed_mx <<- inner_mx
                # reset matrix since it is being 're-set'
                this_inv <<- NULL
        }
        
        # getter / return the matrix used to create this 'object'
        get <- function() passed_mx
        
        setInverse <- function(inv) this_inv <<- inv
        
        getInverse <- function() this_inv
        
        list(set = set, 
             get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## ------------------------------------------------------------------------
## Return a matrix that is the inverse of 'x'
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated ##
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache
## ------------------------------------------------------------------------

cacheSolve <- function(x, ...) {

        this_inverse    <- x$getInverse()
        
        if(!is.null(this_inverse)) {
                message("getting cached data")
        }else{
                original_matrix <- x$get()
                this_inverse <- solve(original_matrix, ...)
                x$setInverse(this_inverse)
        }
        
        return(this_inverse)
}



