library(tidyverse)
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

df = df %>% dplyr::filter(country == "Poland", lubridate::month(date)>=3) %>% 
  dplyr::select(., date, value)
df$country = NULL

df$days = 1:nrow(df)
df$diff = c(0,diff(df$value))
df$procent = (round(df$diff/lag(df$diff,1),3)-1)*100

plot(df$procent)
plot(df$date, df$value)

library(ggplot2)
library(ggthemes)

ggplot(tail(df,15), aes(x = date, y = procent)) +geom_point()+
  geom_smooth()+ theme_stata()

trening = tail(df, 15)
trening
#model1
x <- trening$days
lnN <- log(trening$value)
model1 <- lm(lnN~x)
#y <- exp(predict(model1, newdata = list(x = x), interval = "confidence", level = c(0.9)))
#y


# model2 (loess)
model2= loess(value~days, data= trening, control=loess.control(surface="direct"))

plot(trening$date, trening$value)
lines(trening$date, round(predict(model2),1))
 
#resid(model2)
  
# model 3 (wielomian?)

head(trening)
model3 = lm(value~ (days) + I(days^2) + I(days^3) , data = trening)
plot(round(predict(model3),1))
lines(trening$value)
#resid(model3)

# # model 4 - xgboost
# library(xgboost)
# y <- trening$value
# x <- data.frame(days = trening$days)
# 
# library(caret)
# m_xgboost <- train(x, y, method = 'xgbLinear',
#                    preProcess = c("center", "scale"),
#                    trControl = trainControl(method = "repeatedcv",  number = 5, repeats = 3),
#                    eta=0.1, max_depth=3)
# predict(m_xgboost)-trening$value
# 
# # randomforest:
# library(ranger)
# model5 = ranger(value~days, data= trening, num.trees = 500, mtry=1)
# tt = predict(model5, data = trening, type = "response")
# tt$predictions  

library(nnet)
model4 <- nnet(value ~ days, data = trening, size = 8,  linout = TRUE)
plot(trening$date, trening$value)
points(trening$date, predict(model4, newdata = trening), col='red')


# procentowy:

plot( trening$procent, type= 'l')
model6 = lm(procent ~ days, data = trening)

abline(model6)

procentowy_model = function(model6, prognoza){
procenty = as.numeric(coef(model6)[2])/100
start =trening$diff[nrow(trening)]
for (i in 1:nrow(prognoza)){

  if(i==1)  {
    tmp = round(start[length(start)] + start[length(start)] * procenty , 2)
    start = tmp
  } else {
    tmp = start[length(start)] * procenty + start[length(start)]
    start = c(start, tmp)
  }
    
}
cumsum(start)+trening$value[nrow(trening)]
}

procentowy_model(model6, prognoza)

#library(jsonlite)
# exportJSON <- toJSON(lista)
# write(exportJSON, "test.json")


## stworzenie nowego dejtafrejma

prognoza = data.frame(
  date = seq.Date(from = max(df$date), to = Sys.Date()+14, by = "day"),
  #date = seq.Date(from = Sys.Date(), to = Sys.Date()+15, by = "day"),
           value = NA,
           days = NA,
           diff = NA,
           procent = NA)

dzien1 = as.numeric(min(prognoza$date)-as.Date("2020-03-01"))+1

prognoza$days = dzien1:(dzien1+nrow(prognoza)-1)
prognoza


# wykladniczy:
m1 = exp(predict(model1, newdata = list(x = prognoza$days), interval = "confidence", level = c(0.9)))
# loess
m2 = predict(model2, prognoza)
# wielomian
m3 = predict(model3, prognoza)
# xgboost i random forest / nie zadzialaja bo:
# http://freerangestats.info/blog/2016/12/10/extrapolation

# zostaja nam za to sieci neuronowe
m4 = predict(model4, newdata = prognoza)
m4 = m4[,1]
# model 6 - procentowy
m5 = procentowy_model(model6, prognoza)


wynik = list(data = prognoza$date, exp_1 = m1[,1], exp_2 = m1[,2], loess = m2, 
             polynom = m3, neural_net = m4, persistance = m5)

wynik = do.call(cbind.data.frame, wynik)

library(tidyverse)

df1 = data.frame(data = df[,1], name = "obs", value = df[,2])
colnames(df1)[1] = "data"
df2 = pivot_longer(wynik, cols = -1)

df3 = rbind.data.frame(df1, df2)
head(df3)
colnames(df3)[2] = "model"

zakresy = filter(df3, data %in% (Sys.Date()-14):(Sys.Date()+7)) %>% pull(value) %>% range()
zakresy[1] = 500

library(ggthemes)
p1 = ggplot(df3, aes(x = data, y = value, col = model))+
  geom_line(lwd=2)+
  xlab("")+
  ylab("")+
  #ylim(zakresy)+
  scale_y_log10(limits  = zakresy)+
  annotation_logticks()  +
  xlim(c(Sys.Date()-14, Sys.Date()+7))+
  theme_bw()+
  labs( 
    title = "Prognoza zdiagnozowanych przypadków COVID-19 - Polska", 
    subtitle = paste0("Według danych na dzień: ", max(df$date)), 
    caption = "źródło: John Hopkins University", 
    x = "Data", 
    y = "" 
  ) 

p1

p2 = ggplot(df3, aes(x = data, y = value, col = model))+
  geom_point()+
  xlab("")+
  ylab("")+
  scale_y_continuous(limits  = zakresy)+
  xlim(c(Sys.Date()-14, Sys.Date()+7))+
  theme_bw()

library(gridExtra)
grid.arrange(p1, p2, ncol=2)

