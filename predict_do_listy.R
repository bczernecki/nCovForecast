
library(tidyverse)
#load("dat/cacheData0323.RData")
library("readr")

## Get data
tsConf <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
# tsDeath <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
# tsTesting <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_testing_global.csv"

tsI<-read_csv(file = tsConf)
#tsD<-read_csv(file = tsDeath)


df = pivot_longer(tsI[,-c(1,3,4)], cols = -1, names_to = "date")
colnames(df) = c("country", "date", "value")
df$date = as.Date(df$date, format = "%m/%d/%y")
df = df %>% group_by(country, date) %>% summarise(value = sum(value))

lista = split(df, f = df$country)

lista[[1]]

#library(jsonlite)
# exportJSON <- toJSON(lista)
# write(exportJSON, "test.json")

# debugging:
ctr = "PL"
rawN = lista[[ind]]$value
rawTime = lista[[ind]]$date
inWindow = 10

projSimple<-function(ctr, rawN, rawTime, inWindow=10){
  #rawTime = as.numeric(rawTime)
  nn <- length(rawN)
  ss <- (nn-inWindow+1):nn
  x <- c(rawTime[ss], rawTime[nn]+(1:inWindow))
  lnN <- log(rawN[ss])
  lnN[is.infinite(lnN)]<-NA
  
  if(!all(is.na(lnN))){
  tIn <- rawTime[ss]
  mFit <- lm(lnN~tIn)
  extFit <- predict(mFit, newdata = list(tIn = x), interval = "confidence", level = c(0.9))
  y <- exp(extFit)
  y
  
  
  
  lista = list(predict_date = x, 
             predict_fit = round(y[,1]), 
             predict_lower = round(y[,2]),
             predict_upper = round(y[,3]),
             obs_date = rawTime,
             obs_fit = rawN)
  return(lista)
  }
}

tt = lista[[134]]
head(tt)
ab = projSimple(ctr = tt$country[1], tt$value, tt$date)
names(ab)
ab

bb = lapply(lista, function(x) projSimple(x$country[1], x$value, x$date))
bb = bb[lapply(bb,length)>0]

#bb = do.call(rbind.data.frame, bb)

library(jsonlite)
exportJSON <- toJSON(bb)
exportJSON
write(exportJSON, "tk_czwartek.json")

names(bb)
ind = which(names(bb)=="Poland")
pl = bb[[ind]]

t1 = which(pl$obs_fit>0)[1]-1

plot(pl$predict_date, pl$predict_lower, pch = 19, col = "#00000050",
     xlim = range(c(pl$obs_date[t1], pl$predict_date-6)), 
     #ylim = range(c(pl$obs_fit, pl$predict_fit)) ,
     ylim = c(0,10000),
     xlab = "", 
     ylab = "Confirmed", main = names(bb)[ind])
abline(v=(min(pl$obs_date) : max(pl$predict_date)), lty=2, col='grey')
lines(pl$obs_date, pl$obs_fit)
lines(pl$predict_date, pl$predict_lower, lty=2, col = 'red')
lines(pl$predict_date, pl$predict_upper, lty=2, col = 'red')

polygon(x = c(min(pl$obs_date), max(pl$obs_date), max(pl$obs_date), min(pl$obs_date)),
        y = c(min(pl$obs_fit), 0, max(pl$obs_fit), max(pl$obs_fit)))

polygon(x = c(min(pl$obs_date), max(pl$obs_date), max(pl$obs_date), min(pl$obs_date)),
        y = c(min(pl$obs_fit), 0, max(pl$obs_fit), max(pl$obs_fit)))

# o ile procentowo rosnie z dnia na dzien
mean((diff(pl$obs_fit)/pl$obs_fit)[-1:-42])
# library(rjson)
# tk = fromJSON(file = "tk.json")


pred = data.frame(date = pl$predict_date, pred = pl$predict_lower)
obs = data.frame(date = pl$obs_date, obs = pl$obs_fit)
calosc = left_join(pred, obs)

calosc$pred-calosc$obs
calosc
