## Programming assignment 2 Lexical Scoping - Coursera
## Armazenando o inverso de uma matriz:
## Matriz de invers�o � geralmente um c�lculo dispendioso e pode haver alguma
## benef�cio ao cache o inverso de uma matriz em vez de comput�-lo repetidamente.
## Estes s�o um par de fun��es que s�o usadas para criar um objeto especial que
## armazena uma matriz e armazena em cache o seu inverso.

## Parte 1
## Esta fun��o cria uma "matriz" que pode armazenar em cache o seu inverso.

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
## Esta fun��o calcula o inverso da "matriz" especial criada pela fun��o
## MakeCacheMatrix. Se o inverso j� foi calculado (e o
## Matriz original n�o alterou), ent�o ocorre a recupera��o do inverso do cache.

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