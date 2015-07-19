## makeCacheMatrix stores a matrix X in memory 
## cacheSolve shows the inverse of a matrix if it's in memory or if not calculates the inverse prints it 

## Creates matrix object to cache its inverse.
##Used scoping to store matrices in memory
makeCacheMatrix<- function(X=matrix()){
  inverse<- NULL
  set <- function(Y){
    X<<-Y
    inverse<<-NULL
  }
  get <- function() X
  setinverse<-function(Inverse) inverse<<- Inverse
  getinverse<-function() inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.
##Avoids determinants by installing corpcor package if it's not already installed

cacheSolve<- function(X, ...)
{
  if(require("corpcor")){
    print("corpcor is loaded correctly")
  } else {
    print("trying to install corpcor")
    install.packages("corpcor")
    if(require(corpcor)){
      print("corpcor insalled and loaded")
    } else {
      stop("could not install corpcor")
    }
  }
  inverse<- X$getinverse()
  if(!is.null(inverse)){
    message("matrix in memory")
    return(inverse)
  }
  message("inverse is not in memory so the inverse (if it exists) is going to be computed")
  data<-X$get()
  inverse<-pseudoinverse(data)
  X$setinverse(inverse)
  inverse
}