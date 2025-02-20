library(alabama)

# A and B ------------------------------------------------------------

f <- function(x) {
  return(60*sum(x[1:5]) + 60*sum(x[6:10]) + 60*(x[1] + x[2] + x[3] + x[6] + x[7] + x[8] + x[11]) +
           90*(x[4] + x[5] + x[9] + x[10] + x[11] + 2*x[12]))
}

g <- function(x) {
  h <- numeric(14)
  h[1] <- 6*x[1]-32
  h[2] <- 6*x[6]-8
  h[3] <- 6*x[2]-68
  h[4] <- 6*x[7]-17
  h[5] <- 6*(x[1] + x[3])-56
  h[6] <- 6*(x[6] + x[8])-14
  h[7] <- 6*(x[2] + x[4])-76
  h[8] <- 6*(x[7] + x[9])-19
  h[9] <- 6*(x[3] + x[5] + x[11])-64
  h[10] <- 6*(x[8] + x[12])-16
  h[11] <- 6*(x[4] + x[11] + x[12])-28
  h[12] <- 6*x[9]-7
  h[13] <- 6*(x[5] + x[12])-8
  h[14] <- 6*x[10]-2
  return(h)
}

p0 <- rep(20, 12)
k <- constrOptim.nl(p0, f, hin = g)
k$par <- ceiling(k$par)
print(k$par)
print(f(k$par))

# C & D
g2 <- function(x){
  h <- numeric(2)
  h[1] <- x[4] - 1
  h[2] <- x[5] - 1
  return(h)
}

# C AND D -----------------------------------------------------------------


k <- constrOptim.nl(p0, f, hin = g, heq = g2)
k$par <- ceiling(k$par)
print(k$par)
print(f(k$par))

# E & F
f <- function(x) {
  return(60*sum(x[1:5]) + 60*(x[1]+x[2]+x[3]+x[6]) + 90*(x[4]+x[5]+x[6]+2*x[7]))
}

# E AND F -----------------------------------------------------------------

g <- function(x) {
  h <- numeric(14)
  h[1] <- 6*x[1]-40
  h[2] <- 6*x[2]-85
  h[3] <- 6*(x[1]+x[3])-70
  h[4] <- 6*(x[2]+x[4])-95
  h[5] <- 6*(x[3] + x[5]+x[6])-80
  h[6] <- 6*(x[4] + x[6]+x[7])-35
  h[7] <- 6*(x[5] + x[7])-10
  h[8:14] <- x[1:7]
  return(h)
}

p0 <- rep(20, 7)
k <- constrOptim.nl(p0, f, hin=g)
k$par <- ceiling(k$par)
print(k$par)
print(f(k$par))

##Wage Rate: 
rate <- ((5610 - 5130) / 5610)
percentage <- round(rate * 100, 1)
print(sprintf("%s%%", percentage))

