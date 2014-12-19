##设置一个特殊矩阵对象，并求其逆矩阵（假设矩阵都可逆），
##如果缓存中存在逆矩阵，则从缓存读取，否则重新计算逆矩阵并存入缓存

## makeCacheMatrix函数用于创建可缓存逆矩阵的特殊“矩阵”对象。
makeCacheMatrix <- function(x = matrix()) {
        ## s 是逆矩阵运算结果，首先在创建矩阵对象时候置为空值
        s <- NULL
        ## set属性，设置原始矩阵数据，并清除缓存数据
        set <- function(m1){
                x <<- m1
                s <<- NULL
        }
        ## get属性，获得原始矩阵
        get <- function() x
        ## 缓存逆矩阵
        setsolve <- function(solve) s <<- solve
        ## 获得缓存逆矩阵
        getsolve <- function() s
        ## 返回特殊对象，是一个list对象
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve) 
}

## cacheSolve 函数用于计算上述makeCacheMatrix返回的特殊“矩阵”的逆矩阵。
## 如果已经计算逆矩阵（且尚未更改矩阵），那么cachesolve将检索缓存中的逆矩阵。
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## 参数x为特殊矩阵对象
        ## 首先读取缓存，如果存在逆矩阵，直接返回缓存中的数据
        ## 并显示 "getting cached data" 示意结果是从缓存中读取
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        ## 如果缓存中不存在逆矩阵的数据，则计算solve（）
        data <- x$get()
        s <- solve(data, ...)
        ## 将计算结果存入缓存，返回逆矩阵结果
        x$setsolve(s)
        s     
}
