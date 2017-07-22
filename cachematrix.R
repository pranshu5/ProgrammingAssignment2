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