
## Estas funciones se utilizan para crear un objeto especial
##que almacena una matriz  y la inversa en la memoria caché.

## Esta funcion permite la construccion de una matriz
#que contiene una funcion para:
#establecer el valor de la matriz
#obtener el valor de la matriz
#establecer el valor de la inversa
#obtener el valor de la inversa

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL

     }
     get <- function() {x}
     setInverse <- function(inverse) {inv <<- inverse}
     getInverse <- function() {inv}
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}

## Esta funcion calcula la inversa de la matriz especial creada con la función anterior.
#primero verifica si la inversa ya se ha calculado. Si es así, obtiene la inversa del caché
#y omite el cálculo. De lo contrario, calcula la inversa de los datos y establece el valor de la
#inversa en la memoria caché.

cacheSolve <- function(x, ...) {
     inv <- x$getInverse()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     mat <- x$get()
     inv <- solve(mat, ...)
     x$setInverse(inv)
     inv
        ## Return a matrix that is the inverse of 'x'
}
