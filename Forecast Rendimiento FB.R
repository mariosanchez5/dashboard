library(fpp3)
library(dplyr) 
library(tidyquant)
fb <- tq_get("FB", get = "stock.prices", from = "2018-01-01", to = "2020-07-20")
fb1 <- fb %>% tq_transmute(select = adjusted, mutate_fun = periodReturn,
                           period = "daily",
                           type = "arithmetic", col_rename = "Ra")


#summary(fb) 
fb2= fb1 %>% mutate(date = as_date(date))%>% as_tsibble( index=date)

#Un poco de estadisticas
library(quantmod) 
library(tsbox) 
fb2 %>% features(Ra, median)


fb2 = fb2 %>% mutate(trading_day = row_number()) %>% update_tsibble(index=trading_day, regular=TRUE)


fb2 %>% autoplot(Ra)

fb2 = fb2 %>% filter_index("400" ~ .)
train_fb <- fb2 %>% filter_index(. ~ "600")

fit_fb <- train_fb %>% 
  model(
    Mean = MEAN(Ra),
    Naive = NAIVE(Ra), 
    #Seasonal = SNAIVE(Ra), 
    Drift = RW(Ra ~ drift())
    )

fc_fb <- fit_fb %>% forecast(h=40)

fc_fb %>%
  autoplot(train_fb, level = NULL) + 
  ggtitle("Rendimiento de la accion") + 
  xlab("Day") + ylab("") + 
  guides(colour=guide_legend(title="Forecast"))

fc_fb %>%
  autoplot(fb2, level = NULL) + 
  ggtitle("Rendimiento de la accion") + 
  xlab("Quarter") + ylab("") + 
  guides(colour=guide_legend(title="Forecast"))

fit_fb %>% accuracy()
