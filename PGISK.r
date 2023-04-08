library(fpp3)
library(forecast)
library(fabletools)
library(tidyverse)

set.seed(307005) 

df_series <- as_tsibble(aus_retail) %>% 
  filter(`Series ID` == sample(aus_retail$`Series ID`,1)) %>% 
  select(Month, Turnover) 

df_series %>% autoplot(Turnover) + 
  xlab("Czas") + 
  ylab("Obrót") +
  theme_bw() 

df_series %>% gg_season(Turnover) + 
  xlab("Czas") + 
  ylab("Obrót") +
  theme_bw()

df_series %>% gg_subseries(Turnover) + 
  xlab("Czas") + 
  ylab("Obrót") 


df_series %>% features(Turnover, features = guerrero)
df_series %>% autoplot(sqrt(Turnover))  
df_series %>% autoplot(log(Turnover)) 

tseries::adf.test(df_series$Turnover)
tseries::adf.test(sqrt(df_series$Turnover))
tseries::adf.test(log(df_series$Turnover))

df_series$Turnover <- sqrt(df_series$Turnover)


df_series %>%  model(STL(Turnover ~ trend(window = 9) + season(window = 5), robust = TRUE)) %>%
  components() %>%  autoplot()

tseries::adf.test((df_series %>% model(STL(Turnover ~ trend(window = 9) + season(window = 5), robust = TRUE)) %>% components())$remainder)
acf((df_series %>% model(STL(Turnover ~ trend(window = 9) + season(window = 5), robust = TRUE)) %>% components())$remainder)

# ------------------- 
df_train <- df_series %>% filter(year(Month) < 2011)
df_test <- df_series %>% filter(year(Month) >= 2011)

model_train <- df_train %>% model(Drift = RW(Turnover ~ drift()),
                                  ETS = ETS(Turnover),
                                  NAIVE = NAIVE(Turnover),
                                  SNAIVE = SNAIVE(Turnover))

fc <- model_train %>%
  fabletools::forecast(h = nrow(df_test))

fc %>%
  autoplot(df_series %>% filter(year(Month) >= 2011), level = NULL) +
  labs(y = "Obrót", title = "Prognoza") +
  guides(colour = guide_legend(title = "Metoda"))

fc %>%
  autoplot(df_series, level = NULL) +
  labs(y = "Obrót", title = "Prognoza") +
  guides(colour = guide_legend(title = "Metoda"))

fabletools::accuracy(model_train)
fabletools::accuracy(fc, df_test)

df_train %>% model(ETS(Turnover)) %>%
  gg_tsresiduals() + ggtitle("Reszty - metoda ETS") 

shapiro.test((augment(model_train) %>% filter(.model=='ETS'))$.resid)

# ------------------- 

model_HW = df_train %>% model(
    HW = ETS(Turnover ~ error("M") + trend("M") + season("M")),
    HW_damped = ETS(Turnover ~ error("M") + trend("Md") + season("M")))

fc_HW <- model_HW %>% fabletools::forecast(h=nrow(df_test)) 

fc_HW %>%
  autoplot(df_series %>% filter(year(Month) >= 2011), level = NULL) +
  labs(y = "Obrót", title = "Prognoza") +
  guides(colour = guide_legend(title = "Metoda"))

fc_HW %>%
  autoplot(df_series, level = NULL) +
  labs(y = "Obrót", title = "Prognoza") +
  guides(colour = guide_legend(title = "Metoda"))

fabletools::accuracy(model_HW)
fabletools::accuracy(fc_HW, df_test)

df_train %>% model(ETS(Turnover ~ error("M") + trend("M") + season("M"))) %>%
  gg_tsresiduals() + ggtitle("Reszty - metoda HW") 

shapiro.test((augment(model_HW) %>% filter(.model=='HW'))$.resid)

# dekompozycja

df_train_ts <- ts(df_train$Turnover, start = c(1982,4), frequency = 12)
df_test_ts <- ts(df_test$Turnover, start = c(2011,1), frequency = 12)

fc_stl_ets_train <- df_train_ts %>%
  stlm(s.window = 13, robust = TRUE, method = "ets", lambda = BoxCox.lambda(df_train_ts)) %>%
  forecast(h = nrow(df_test), lambda = BoxCox.lambda(df_train_ts))

autoplot(fc_stl_ets_train, level=NULL) +
  labs(y = "Obrót", title = "Prognoza")

forecast::accuracy(fc_stl_ets_train, df_test_ts)

# ------------------- 

model_ARIMA <- df_train %>% model(ARIMA = ARIMA(Turnover))

fc_ARIMA <- model_ARIMA %>%
  fabletools::forecast(h = nrow(df_test))

fc_ARIMA %>%
  autoplot(df_series %>% filter(year(Month) >= 2011), level = NULL) +
  labs(y = "Obrót", title = "Prognoza") +
  guides(colour = guide_legend(title = "Metoda"))

fc_ARIMA %>%
  autoplot(df_series, level = NULL) +
  labs(y = "Obrót", title = "Prognoza") +
  guides(colour = guide_legend(title = "Metoda"))

fabletools::accuracy(model_ARIMA)
fabletools::accuracy(fc_ARIMA, df_test)

df_train %>% model(ARIMA(Turnover)) %>%
  gg_tsresiduals() + ggtitle("Reszty - ARIMA") 

# ------------------- 

model_THETA_NNETAR <- df_train %>% 
  model(NNETAR = NNETAR(Turnover),
        THETA = THETA(Turnover))

fc_THETA_NNETAR <- model_THETA_NNETAR %>%
  fabletools::forecast(h = nrow(df_test))

fc_THETA_NNETAR %>%
  autoplot(df_series %>% filter(year(Month) >= 2011), level = NULL) +
  labs(y = "Obrót", title = "Prognoza") +
  guides(colour = guide_legend(title = "Metoda"))

fc_THETA_NNETAR %>%
  autoplot(df_series, level = NULL) +
  labs(y = "Obrót", title = "Prognoza") +
  guides(colour = guide_legend(title = "Metoda"))

fabletools::accuracy(model_THETA_NNETAR)
fabletools::accuracy(fc_THETA_NNETAR, df_test)

df_train %>% model(NNETAR(Turnover)) %>%
  gg_tsresiduals() + ggtitle("Reszty - metoda NNETAR") 
