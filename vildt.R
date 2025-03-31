library(dplyr)
library(tidyquant)

udvalgte <- read.csv("data/monthly_log_returns_2017.csv")

vec <- as.vector(colnames(udvalgte))
vec <- vec[2:length(vec)]


Ra <- vec %>%
  tidyquant::tq_get( get = "stock.prices",
                     from = "2021-01-01",
                     to = base::Sys.Date()) %>%
  dplyr::group_by(symbol) %>%
  tidyquant::tq_transmute(select = adjusted,
                          mutate_fun = periodReturn,
                          period = "monthly",
                          col_rename = "Ra")

Rb <- "VUSA.AS" %>%
  tq_get(get  = tq"stock.prices",
         from = "2021-01-01",
         to   = base::Sys.Date()) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "Rb")

Rb

RaRb <- dplyr::left_join(Ra, Rb, by = c("date" = "date"))
RaRb

RaRb_capm <- RaRb %>%
  tidyquant::tq_performance(Ra = Ra,
                 Rb = Rb,
                 performance_fun = table.CAPM)
RaRb_capm

#
