sharpe_optim <- function(path_to_csv = "data/data_udvalgt.csv", rf_annual = 0.044) {
  library(tidyverse)
  library(corpcor)

  # 1. Indlæs og forbered data
  returns <- read_csv(path_to_csv) %>%
    rename(Date = 1) %>%
    mutate(Date = as.Date(Date))

  returns_mat <- returns %>%
    select(-Date) %>%
    drop_na() %>%
    as.matrix()

  mu_monthly <- colMeans(returns_mat)
  mu_annual <- mu_monthly * 12
  sigma <- corpcor::cov.shrink(returns_mat) * 12

  # 2. Objektivfunktion: max Sharpe ratio
  sharpe_obj <- function(w) {
    port_ret <- sum(w * mu_annual)
    port_vol <- sqrt(t(w) %*% sigma %*% w)
    -(port_ret - rf_annual) / port_vol
  }

  # 3. Optimér
  dres <- optim(
    par = rep(1 / length(mu_annual), length(mu_annual)),
    fn = sharpe_obj,
    method = "L-BFGS-B",
    lower = rep(0, length(mu_annual)),
    upper = rep(1, length(mu_annual)),
    control = list(fnscale = 1)
  )

  w_opt <- dres$par / sum(dres$par)
  port_ret <- sum(w_opt * mu_annual)
  port_vol <- sqrt(t(w_opt) %*% sigma %*% w_opt)
  sharpe <- (port_ret - rf_annual) / port_vol

  # 4. Returnér tibble
  tibble(
    Ticker = colnames(returns_mat),
    Weight = round(w_opt, 4),
    AnnualReturn = round(mu_annual, 4),
    PortfolioReturn = round(port_ret, 4),
    PortfolioVolatility = round(port_vol, 4),
    SharpeRatio = round(sharpe, 4),
    Method = "Sharpe Ratio"
  )
}



#
# # plot --------------------------------------------------------------------
#
# library(scales)
#
#
# # 1. Indlæs og forbered
# returns_xts <- read_csv("data/data_udvalgt.csv") %>%
#   rename(Date = 1) %>%
#   mutate(Date = as.Date(Date)) %>%
#   drop_na()
#
# dates <- returns_xts$Date
# returns_mat <- returns_xts %>% select(-Date) %>% as.matrix()
#
# # 2. Beregn portefølje og benchmark
# port_returns <- as.numeric(returns_mat %*% w_opt)
# cum_port <- 100 * exp(cumsum(port_returns))
#
# benchmark_name <- "EQQQ.DE"
# benchmark_returns <- returns_mat[, benchmark_name]
# cum_benchmark <- 100 * exp(cumsum(benchmark_returns))
#
# # 3. Fjern benchmark fra asset-matrix inden plot
# asset_names <- setdiff(colnames(returns_mat), benchmark_name)
# cum_assets <- apply(returns_mat[, asset_names], 2, function(x) 100 * exp(cumsum(x)))
#
# # 4. Saml alt i ét plot-dataframe
# performance_df <- as_tibble(cum_assets)
# performance_df$Date <- dates
#
# # Tilføj portefølje og benchmark
# performance_df <- performance_df %>%
#   mutate(Portfolio = cum_port,
#          Benchmark = cum_benchmark)
#
# # 5. Omform til long format
# performance_long <- performance_df %>%
#   pivot_longer(-Date, names_to = "Asset", values_to = "Value") %>%
#   mutate(Type = case_when(
#     Asset == "Portfolio" ~ "Portfolio",
#     Asset == "Benchmark" ~ "Benchmark",
#     TRUE ~ "Asset"
#   ))
#
# # 6. Plot med farver og lagorden
# ggplot() +
#   geom_line(
#     data = performance_long %>% filter(Type == "Asset"),
#     aes(x = Date, y = Value, group = Asset),
#     color = "gray", size = 0.5, alpha = 0.7
#   ) +
#   geom_line(
#     data = performance_long %>% filter(Type == "Benchmark"),
#     aes(x = Date, y = Value),
#     color = "red", size = 1.2
#   ) +
#   geom_line(
#     data = performance_long %>% filter(Type == "Portfolio"),
#     aes(x = Date, y = Value),
#     color = "green", size = 1.2
#   ) +
#   labs(title = "📊 Portefølje vs Benchmark vs Aktiver",
#        y = "Værdi (Start = 100 kr)", x = NULL) +
#   scale_y_continuous(labels = dollar_format(suffix = " kr", prefix = "")) +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none",
#         plot.title = element_text(face = "bold"))
#
#
#
#
#
