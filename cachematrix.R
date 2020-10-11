makeCacheMatrix <- function(x = matrix()) {
  inversa<- NULL  ##Null Variable
  ##name the functions for matrix and the inverse.
  set<-function(y){     ##Enter a matrix
    x<<-y   ##Give new value to the original matrix. X is equal to Y matrix
    inversa <<- NULL ##If we put new matrix the inverse will be null

  }
  get <-function(){x}  ##show the matrix generated
  setInverse <- function(inversacalculada){inversa<<-inversacalculada}  #inversa will be the inverse calculated
  getInverse<- function(){inversa} #show the value of inversa
  
  list(set = set, get = get,setInverse = setInverse, getInverse = getInverse) ##show the list with all the functions of the object.
    }


cacheSolve <- function(x, ...) {
  inversa<-x$getInverse()     ###Write the inverse matrix values
  ##If it doesn't have any value we return the inverse
  if(!is.null(inversa)){
    return(inversa)
  }
  
  data<-x$get()  ##show the value of the matrix x
  inversa<-solve(data,...) ##calculate the inverse of the matrix using the Solve function
  x$setInverse(inversa)  ##set the calculated inverse matrix to the X variable
  inversa ## Return a matrix that is the inverse of 'x'
}

x<-makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
cacheSolve(x)
