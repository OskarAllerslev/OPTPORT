library(quantmod)

test_prices <- getSymbols("XACTC25.CO",
                          from       = "2022-01-01",
                          to         = "2023-01-01",
                          src        = "yahoo",
                          auto.assign = FALSE)
head(test_prices)
