library(dplyr)
library(tidyquant)

Ra <- c("AAPL", "NFLX") %>%
  tidyquant::tq_get( get = "stock.prices",
                     from = "2021-01-01",
                     to = base::Sys.Date()) %>%
  dplyr::group_by(symbol) %>%
  tidyquant::tq_transmute(select = adjusted,
                          mutate_fun = periodReturn,
                          period = "monthly",
                          col_rename = "Ra")

Rb <- "XLK" %>%
  tq_get(get  = "stock.prices",
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

# portfolio example



stock_returns_monthly <- c("AAPL", "GOOG", "NFLX") %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") %>%
  dplyr::group_by(symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "Ra")
stock_returns_monthly

baseline_returns_monthly <- "XLK" %>%
  tq_get(get  = "stock.prices",
         from = "2010-01-01",
         to   = "2015-12-31") %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "monthly",
               col_rename = "Rb")
baseline_returns_monthly

# wts_map <- tibble(
#   symbols = c("AAPL", "NFLX"),
#   weights = c(0.5, 0.5)
# )
# wts_map
#
# stock_returns_monthly %>%
#   tq_portfolio(assets_col  = symbol,
#                returns_col = Ra,
#                weights     = wts_map,
#                col_rename  = "Ra_using_wts_map")
wts <- c(0.5, 0.0, 0.5)
portfolio_returns_monthly <- stock_returns_monthly %>%
  tq_portfolio(assets_col  = symbol,
               returns_col = Ra,
               weights     = wts,
               col_rename  = "Ra")
portfolio_returns_monthly

# merging

RaRb_single_portfolio <- left_join(portfolio_returns_monthly,
                                   baseline_returns_monthly,
                                   by = "date")
RaRb_single_portfolio


RaRb_single_portfolio %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)














































