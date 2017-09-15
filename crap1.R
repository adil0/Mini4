del <- 0.8
g <- function(x,y){
  ret_val <- exp(-x-y-del*x*y)*((-1-del*x)*(-1-del*y) + (-del))
  return(ret_val)
}

x<- runif(100)
y <- runif(100)

g(x,y)


exp(-x-y-del*xy)(-1-del*y)



exp(-x-y-del*xy)*((-1-del*x)(-1-del*y) + (-del))


exp(-x-y-del*xy)(-1-del-del*x)


dslnex <- function(x) {
  y <- numeric(1)
  y[1] <- x[1]^2 + x[2]^2 - 2
  y
}

xstart <- c(2,1)
nleqslv(xstart, dslnex, control=list(btol=.01))



f <- function (x,a) x-a
AA <- uniroot.all(f,c(0,10),a=1/3)



x <- 0:64/64
y <- sin(3*pi*x)
plot(x, y, type = "l", col = "blue",
     main = "points with bg & legend(*, pt.bg)")
points(x, y, pch = 21, bg = "white")
plot(x, y, type = "n")
legend("bottomright", "(x,y)", pch = 1, title = "bottomright")
