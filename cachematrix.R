## Put comments here that give an overall description of what your
## functions do
## <bogosai> my function do as per assignment: checking if initial matrix is already in cache 
## and if yes and INVERSE is there then we use it, otherwise we save matrix and inverse to cache. 
## At lease is this is what it is doing.

## Write a short comment describing this function

##<bogosai> output is a list if functions to set/get matrix and inverse to/from cache

makeCacheMatrix <- function(x = matrix()) { #matrix input is managed in next function
        m <- NULL
        v <- NULL
        #this is managing set/get for matrix
        set <- function(mtr) v <<- mtr
        get <- function() v
        #this is managing set/get for inverse
        setsolve <- function(slv) m <<- slv
        getsolve <- function() m
        list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { #provide matrix as input, e.g. matrix(1:4,2,2)
        ## Return a matrix that is the inverse of 'x'
        mc<- makeCacheMatrix(x)
        v <- mc$get()
        m <- mc$getsolve()
        #checking whether matrix is in cache and inverse already exists. If both are TRUE then
        #getting inverse from cache
        if (identical(v,x) & !is.null(m)) {
                message("getting cached data")
                return(m)
        } else {
                #saving matrix to cache
                mc$set(x)
                data <- mc$get()
                m<-solve(data,...) #inversing matrix
                mc$setsolve(m) #saving inverse to cache
                mc$getsolve() #output inverse from cache
        }
}
