library(astsa)
library(tidyverse)


plot(lap[,3], main = "Cardiovascular Mortality")
decomp = decompose(lap[,3])
acf(diff_52, type = "partial", lag.max = 104)



mod1 = sarima(lap[,1], 2, 0, 0, 1, 1, 0, S = 52, xreg = cbind(lap[,11], lap[,10]) )
mod2 = sarima(lap[,1], 2, 0, 0, 1, 1, 0, S = 52)



sarima.for(lap[,3], 52, 2, 0, 0, 1, 1, 0, 52)

library(tidyverse)
nba.dat = read.csv('Team_stats_per_game.csv')


# filter to start in 1980 (when 3 pointers started)
# and create a response variable - 3 point attempts, averaged across teams
# and predictor - mean 3 point conversion pct.
nba.processed = nba.dat %>% 
  filter(season > 1979) %>%
  group_by(season) %>%
  summarise(y = mean(x3pa_per_game), x = mean(x3p_percent))

# make the response into a time series object - which clearly has an increasing trend
# inspect the lag 1 differences for stationarity - looks good
yt = as.ts(nba.processed$y, start = 1980, end = 2022)
plot(diff(yt))
(acf(yt, type = "partial")) # order one AR model seems appropriate


m0 = arima(yt, order = c(1, 1, 0)) # fit AR model with one past period to the differenced data
predict(m0, n.ahead = 5) # predict 5 years into the future



xt = as.ts(nba.processed$x, start = 1980, end = 2022)
m1 = arima(yt, order = c(1, 1, 0), xreg = xt)
predict(m1, n.ahead = 5, newxreg = window(xt, start = 39))


