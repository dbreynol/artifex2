library(astsa)
library(tidyverse)


plot(lap[,3], main = "Cardiovascular Mortality")
decomp = decompose(lap[,3])
acf(diff_52, type = "partial", lag.max = 104)



mod1 = sarima(lap[,1], 2, 0, 0, 1, 1, 0, S = 52, xreg = cbind(lap[,11], lap[,10]) )
mod2 = sarima(lap[,1], 2, 0, 0, 1, 1, 0, S = 52)



sarima.for(lap[,3], 52, 2, 0, 0, 1, 1, 0, 52)




