## define makeCacheMatrix function that contains a list of 4 functions

makeCacheMatrix<-function(x=matrix()) {
		inverse<-NULL
		set<-function(y) {
		x<<-y
		inverse <<- NULL
        }
		get<-function() x
		setinverse<-function(inv) inverse<<-inv
		getinverse<-function() inverse
        	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

##define CacheSolve function that computes-- 
##the inverse of the “matrix” returned by makeCacheMatrix above
##solve() for calculating the inversion of a matrix

cacheSolve<-function(x, ...) {
		inverse<-x$getinverse()
		if(!is.null(inverse)) {
            message("getting cached data")
		return(inverse)
        }
		mat<-x$get()
		inverse<-solve(mat, ...)
		x$setinverse(inverse)
		inverse
}

##Test the makeCacheMatrix and cacheSolve funcstions

testmatrix<- makeCacheMatrix(matrix(2:9, 3, 3))

testmatrix$get()

testmatrix$getinverse()

cacheSolve(testmatrix)

testmatrix$getinverse()







