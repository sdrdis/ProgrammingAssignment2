##  OK, Here goes...
##This function will create a "special function" list that is called upon by the
##  cacheSolve function.  Each element of the resulting list will create conditions
##  needed to either create or recall a cached inverted matrix.
makeCacheMatrix <- function(X = matrix()) {
        
        #Determines the matrix dimensions.
        Dims <- dim(X)
        x <- Dims[1]
        y <- Dims[2]
        
        # Tests if the input matrix is square.  If not, a message is returned and function
        # will end. "Bullet-Proofs" the code so that only a square matrix can be used.
        if (x != y) {
                message("matrix is not square.")
                stop("Must use a square, non-singular input matrix.") 
        }
        
        #Creates an empty matrix with dimensions similar to that of the input matrix.
        SqMat <- matrix(,x,y)
        
        #Sets up the "special matrix" list needed for the cachesolve function.
        setMat <- function(Y) {
                X <<- Y
                SqMat <<- matrix()
        }
        getMat <- function() X
        setInverse <- function(solve) SqMat <<- Inverse
        getInverse <- function() SqMat
        list(setMat = setMat, getMat = getMat,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function will return the inverted matrix using the "special matrix" list
##  output from "makeCacheMatrix" function.  If the inverted matrix does not previously
##  exist, then the function will use the built-in "solve" function to invert the matrix.
cacheSolve <- function(X, ...) {
        #Gets the stored square matrix that is inverted.
        SqMat <- X$getInverse()
        #Tests to determine if the stored matrix is empty or not empty.  If not empty,
        #  then it retreives the inverted matrix from the cache.
        if(!is.na(SqMat[1,1])) {
                message("getting cached data")
                return(SqMat)
        }
        #The rest of this code will invert the matrix, since the if-then condition above for
        # a stored inverted matrix does not exist yet, then returns the inverted matrix.
        MatData <- X$getMat()
        SqMat <- solve(MatData, ...)
        X$setMat(SqMat)
        SqMat
}
