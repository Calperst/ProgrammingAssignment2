## Programming assignment 2 Lexical Scoping - Coursera
## Armazenando o inverso de uma matriz:
## Matriz de inversão é geralmente um cálculo dispendioso e pode haver alguma
## benefício ao cache o inverso de uma matriz em vez de computá-lo repetidamente.
## Estes são um par de funções que são usadas para criar um objeto especial que
## armazena uma matriz e armazena em cache o seu inverso.

## Parte 1
## Esta função cria uma "matriz" que pode armazenar em cache o seu inverso.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Parte 2
## Esta função calcula o inverso da "matriz" especial criada pela função
## MakeCacheMatrix. Se o inverso já foi calculado (e o
## Matriz original não alterou), então ocorre a recuperação do inverso do cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}