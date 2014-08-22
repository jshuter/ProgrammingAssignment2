#----------------------------------------------------------------------------
## Assignment: Caching the Inverse of a Matrix
#----------------------------------------------------------------------------

# Matrix inversion is usually a costly computation and their may be some benefit to 
# caching the inverse of a matrix rather than compute it repeatedly 

# This script contains functions that cache the inverse of a matrix.

# 1 - test() : can be used to test the functions and insure they function properly 
#            : I use the technique of getting the inverse, and then getting the inverse of the inverse 
#            : The original Matrix should be returned if the functions are working properly 

# 2 - makeCacheMatrix() - used like an Closure or Object that will ...
#            : store a matrix 
#            : return the matrix 
#            : create & store the Inverse (via Solve())
#            : return the Inverse (via cache - create should only be done once)

# 3 - cacheSolve() - used as main interface to getting of the Cached Inverse Matix

#----------------------------------------------------------------------------------
# test() - used to evaluate the two main functions as per assignment instructions 
#----------------------------------------------------------------------------------

test <- function(){ 

        #------------------------
        # get the inverse 3 times
        #------------------------
        
        cache=makeCacheMatrix()
        
        m1=matrix(sample(1:25,25, replace=TRUE),5,5)
        print (m1)
        cache$set(m1)  # Setting #1
        
        inv=cacheSolve(cache);
        print (inv); 
        
        inv=cacheSolve(cache);
        print (inv); 
        
        inv=cacheSolve(cache);
        print (inv); 
        
        # make a new matrix
        m99=matrix(sample(1:16,16, replace=TRUE),4,4)
        print (m99)
        cache$set(m99)  #Setting #2 
        
        # cache - matrix has changed ... 
        inv=cacheSolve(cache);
        print (inv); 
        
        # cache - matrix has changed ... 
        inv=cacheSolve(cache);
        print (inv); 
        
        #------------------------
        # inverse of the inverse
        #------------------------

        m1=cacheSolve(cache)
        print (m1)
        cache$set(m1) # Setting # 3
        
        inv=cacheSolve(cache);
        print (inv);         
}


#-----------------------------------------------------------------------
# This is the main Closure / Object for handling operations on a Matrix 
#-----------------------------------------------------------------------

makeCacheMatrix <- function(passed_mx = matrix()) {

        this_inv <- NULL
        counter <- 0 
        
        # setter / create this 'Closure/Object', save the matrix 
        
        set <- function(inner_mx) {
                counter <<- counter + 1
                message('setting ...')
                message(paste('This is setting number : ',counter))
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
## This Returns a matrix that is the inverse of 'x'
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve will retrieve the 
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

#----------------------------------------------------------------------------
# end 
#----------------------------------------------------------------------------



