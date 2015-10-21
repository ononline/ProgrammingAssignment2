## The two functions below have as objective to encapsulate a matrix and store
##  it's inverse in cache, so that the calculation don't need to be remade

#####

## makeCacheMatrix creates a new object that encapsulate the x matrix given as
##  argument, giving it the capability to store in cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## This is where the inverse matrix will be stored
        inv <- NULL 
        
        ## Used to change the matrix
        set <- function(y) { 
                x <<- y ## Changes the X matrix for this object
                inv <<- NULL ## Resets the inverse so it can be recalculated
        }
        
        ## Returns the matrix for any use
        get <- funcion () x 
        
        ## Updates the inverse
        setInverse <- function(inverse){
                inv <<- inverse
        }
        
        ## Returns the inverse matrix
        getInverse <- function(){
                inv
        } 
        
        ## Return of the makeCacheMatrix function, a list containing the 
        ##  functions to operate the encapsulated matrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse) 

}

#####

## cacheSolve verifies if the matrix have a cached inverse then returns it
##  or calculates it, cache it and then returns it if not.

cacheSolve <- function(x, ...) {
        
        ## Retrieves the cached inverse
        inv <- x$getInverse()
        
        ## Verifies the cached inverse (if inverse is not null)
        if (!is.null(inv)) {
                ## If the inverse was already calculated, then it is returned
                message("Getting cached data") ## As the example in the assignment
                return(inv)
        }
        
        ## If the code gets here the inverse was not calculated.
        
        ## Retrieves the original matrix
        mat <- x$get()
        
        ## Solves the inverse for the original matrix
        inv <- solve(mat, ...)
        
        ## Caches the calculated inverse matrix
        x$setInverse(inv)
        
        ## Return of the cacheSolve function, the inversed matrix
        inv
}
