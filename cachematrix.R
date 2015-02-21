## makeCacheMatrix creates a list containing a  function to set a matrix (input) , get matrix , setinverse and get inverse

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL                                  ##Sets the inverse,i, to NULL
        set<-function(y){
                x<<-y                            ##Sets the matix 'x' to a matrix 'y'
                i<<-NULL                         ##sets the inverse to NULL
        }
        get<-function() x                        ##Returns matrix 'x'
        setinverse<-function(solve) i<<- solve   ##Sets the inverse,i, to solve
        getinverse<-function() i                 ##Returns the inverse 'i'
        list(set=set, get=get,                   
             setinverse=setinverse,
             getinverse=getinverse)
}


## cacheSolve prints the inverse if its already computed else it computes the inverse and prints it

cacheSolve <- function(z, ...) {
        i <- z$getinverse()                    ##Sets the inverse,i, to inverse which is already computed (if computed else it is NULL)
        if(!is.null(i)) {                      ##Checks if inverse,i, is NULL or not
                message("getting cached data") 
                return(i)                      ##Returns the already computed inverse
        }
        data <- z$get()                        ##Stores the matrix in data
        i <- solve(data, ...)                  ##Sets inverse ,i, to the inverse obtained from solve  
        z$setinverse(i)                        ##inverse is stored in the function setinverse()
        i                                      ## Return a matrix that is the inverse of 'x'
}
