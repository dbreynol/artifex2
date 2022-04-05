library(astsa)

window(globtempl, start = 2014)



y = arima.sim(n=10, list(ar = c(.9), sd = 0.5))

window(y, start = 2)


cor( window(y, start = 2), window(y, end = 9) )

cor( as.numeric( window(y, start = 2)), as.numeric ( window(y, end = 9) ) )
(acf(y, type = "correlation"))

x1 = as.numeric( window(y, start = 2))
x2 = as.numeric ( window(y, end = 9) )

xbar = mean(as.numeric(y))

sum ( (x1 - xbar ) * (x2 -  xbar ) ) /  ( sum ( (y - mean(y))^2 ) )


cor(x2, x1)
 