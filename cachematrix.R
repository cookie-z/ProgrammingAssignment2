
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 此函数用于创建可缓存逆矩阵的特殊“矩阵”对象。
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(m1){
                x <<- m1
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve) 
}


## Write a short comment describing this function
## 此函数用于计算上述makeCacheMatrix返回的特殊“矩阵”的逆矩阵。
## 如果已经计算逆矩阵（且尚未更改矩阵），那么cachesolve将检索缓存中的逆矩阵。
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s     
}

