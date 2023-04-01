# Ambiente ----
setwd("atividades-R/")
rm(list = ls())

# Exercícios ----
## Função que soma N números ----
numeros <- seq(1, 20, 2)

numeros <- c(1, 2, 3, 4, NA)

soma <- function(lista, na.rm = F) {
  total <- 0
  for (num in lista) {
    if (na.rm) {
      if (!is.na(num)) {
        total <- total + num
      }
    } else {
      total <- total + num
    }
  }
  return(total)
}

soma(numeros, T)

## Fatorial ----

fatorial <- function(n) {
  produto <- 1
  for (i in 1:n) {
    produto <- produto * i
  }
  return(produto)
}

fatorial_recursivo <- function(n) {
  if (n == 1) {
    return(1)
  } else {
    return( n * fatorial_recursivo(n-1))
  }
}

fatorial_recursivo(5)

## Fizzbuzz ----

n <- 35
fizzbuzz <- function(n) {
  if ((n %% 5 == 0) & (n %% 3 == 0)) {
    print('fizzbuzz')
  } else if (n %% 5 == 0) {
    print('fizz')
  } else if (n %% 3 == 0) {
    print('buzz')
  } else {
    print(n)
  }
}
fizzbuzz(45)

## Escadas do Super Mário ----

### Escada que desce ----
downstairs <- function(n) {
  for (i in 1:n) {
    for (j in 1:i) {
      cat("#")
    }
    cat("\n")
  }
}
downstairs(4)

### Escada que sobe ----
upstairs <- function(n) {
  for (i in 1:n) {
    for (j in 1:(n - i + 1)) {
      cat(" ")
    }
    
    for (k in 1:i) {
      cat("#")
    }
    cat("\n")
  }
}
upstairs(4)

### Piramide ----
piramide <- function(n) {
  for (i in 1:n) {
    for (j in 1:(n - i + 1)) {
      cat(" ")
    }
    for (k in 1:i) {
      cat("#")
    }
    
    cat(" ")
    
    for (l in 1:i) {
      cat("#")
    }
    
    cat("\n")
  }
}

piramide(10)

## Fibonacci ----
fibonacci <- function(n) {
  sequencia_final <- c(1, 1)
  if (n == 1) {
    return(1)
  } else {
    for (i in 2:n) {
      soma <- sequencia_final[1] + sequencia_final[2]
      sequencia_final[1] <- sequencia_final[2]
      sequencia_final[2] <- soma
    }
  }
  return(sequencia_final[1])
}

fibonacci(50)

fibonacci2 <- function(n) {
  if (n == 1 | n == 2) {
    return(1)
  } else {
    return(fibonacci2(n - 1) + fibonacci2(n - 2))
  }
}

fibonacci_sequencia <- function(n) {
  sequencia <- c(1, 1)
  if (n == 1) {
    return(1)
  } else {
    for (i in 2:n) {
      soma <- sequencia[i] + sequencia[i - 1]
      sequencia <- c(sequencia, soma)
    }
  }
  return(sequencia[1:n])
}

fibonacci_sequencia(5)







