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