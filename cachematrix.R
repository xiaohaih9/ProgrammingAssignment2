#This function creates a special "matrix" object that can cache its inver
makeCacheMatrix <- function(x=matrix(),...){
  m<-NULL
  set<-function(y){
    x<<-y  #<<- operator can be used to assign a value to an object in an environment that is different from the current environment.
    m<<-NULL
  }
  get<-function() x
  setM<-function(solve) m<<-solve
  getM<-function() m
  list(set=set,get=get,setM=setM,getM=getM)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
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
 ## Return a matrix that is the inverse of 'x'

}
