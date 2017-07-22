##The below makecacheMatrix function takes creates a matrix and sets it primary values.
##This function set values of matrix variable x and get the values if already inversion
##is performed.


makecacheMatrix<-function(x=matrix()){
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
    get<-function() x
    setinversion<-function(inversion) i<<-inversion
    getinversion<-function() i
    list(set=set, get=get,
         setinversion=setinversion,
         getinversion=getinversion)
}
##The below cachesolve function performs the inversion on the matrix and also verifies the values.
##If the inversion is already performed the matrix retrives the cached values.Foe new matrix
##the cachesolve function performs the inversion using solve function of R.

cachesolve<-function(x,...){
  i<-x$getinversion()
  if(!is.null(i)){
    message("getting cache inversion")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinversion(i)
  return(i)  
}

##Below is a sample solution to the function

##getmatrix<-makecacheMatrix(matrix(c(1:4),2,2))
##> getmatrix$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cachesolve(getmatrix)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

