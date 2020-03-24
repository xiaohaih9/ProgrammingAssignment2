makeCacheMatrix <- function(x=matrix(),...){
  m<-NULL
  set<-function(y){
    x<<-y 
    m<<-NULL
  }
  get<-function() x
  setM<-function(solve) m<<-solve
  getM<-function() m
  list(set=set,get=get,setM=setM,getM=getM)
}

cacheSolve <- function(x, ...) {
  m <- x$getM()
  if(!is.null(m)){
    message("TO cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setM(m)
  m
 
}
