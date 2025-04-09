source("cvar.R")
source("sharpe_opt.R")
source("plot_portfolio_vs_benchmark.R")

cvar_result <- mean_cvar_optim()
sharpe_result <- sharpe_optim()





cvar_result
sharpe_result



result <- sharpe_optim_cardinality(
  path_to_csv = "data/monthly_log_returns.csv",
  rf_annual   = 0.044,
  max_assets  = 10,   # ønsket kardinalitet
  search_size = 5000  # evt. større for bedre solutions
)

# Se valgt portefølje
result$weights_table
# Se performance
result$metrics



plot_portfolios_vs_benchmark(sharpe_result, cvar_result, benchmark_name = "EQQQ.DE")



# tabel -------------------------------------------------------------------

summarize_performance <- function(weights, returns_mat, rf_annual = 0.044) {
  mu_monthly <- colMeans(returns_mat)
  mu_annual <- mu_monthly * 12
  sigma <- corpcor::cov.shrink(returns_mat) * 12

  port_ret <- sum(weights * mu_annual)
  port_vol <- sqrt(t(weights) %*% sigma %*% weights)
  sharpe <- (port_ret - rf_annual) / port_vol

  tibble(
    `Årligt afkast` = round(port_ret, 4),
    `Årlig volatilitet` = round(port_vol, 4),
    `Sharpe Ratio` = round(sharpe, 4)
  )
}
# Fjern dato og benchmark
returns <- read_csv("data/monthly_log_returns.csv") %>%
  rename(Date = 1) %>%
  mutate(Date = as.Date(Date)) %>%
  drop_na()

returns_mat <- returns %>% select(-Date) %>% as.matrix()

# Benchmark return
benchmark_name <- "EQQQ.DE"
benchmark_weights <- rep(0, ncol(returns_mat))
names(benchmark_weights) <- colnames(returns_mat)
benchmark_weights[benchmark_name] <- 1

# Performance summaries
perf_table <- bind_rows(
  Sharpe = summarize_performance(sharpe_result$Weight, returns_mat),
  CVaR = summarize_performance(cvar_result$Weight, returns_mat),
  Benchmark = summarize_performance(benchmark_weights, returns_mat),
  .id = "Portefølje"
)

perf_table





indtjeningscalc <- function(multiplier = 1, profit_pct = 1, rf = 0.044) {
  cap <- 100000 * multiplier

  # Beregn for hver portefølje i perf_table
  u <- perf_table %>%
    mutate(
      Forventet_afkast = `Årligt afkast`,
      SD = `Årlig volatilitet`,
      Mean_Profit = pmax(cap * Forventet_afkast * profit_pct, 0),
      Lower_CI = pmax(cap * (Forventet_afkast - 1.96 * SD) * profit_pct, 0),
      Upper_CI = cap * (Forventet_afkast + 1.96 * SD) * profit_pct
    ) %>%
    select(Portefølje, Mean_Profit, Lower_CI, Upper_CI)

  return(u)
}


indtjeningscalc(multiplier = 10, profit_pct = 0.1)



evaluate_all_metrics <- function(perf_long, benchmark_col = "Benchmark", rf = 0.044) {
  library(tidyverse)
  library(PerformanceAnalytics)
  library(xts)

  # Konverter til liste med xts-objekter
  perf_xts_list <- perf_long %>%
    pivot_wider(names_from = Portfolio, values_from = returns) %>%
    mutate(date = as.Date(date)) %>%
    column_to_rownames("date") %>%
    as.xts()

  portfolios <- colnames(perf_xts_list)

  # Brug benchmark til beta
  benchmark_returns <- perf_xts_list[, benchmark_col]

  # Beregn alle nøgletal
  metrics <- lapply(portfolios, function(pf) {
    Ra <- perf_xts_list[, pf]
    tibble(
      Portfolio = pf,
      AnnualizedReturn = Return.annualized(Ra, scale = 12)[1],
      AnnualizedStdDev = StdDev.annualized(Ra, scale = 12)[1],
      SharpeRatio = SharpeRatio(Ra, Rf = rf / 12, scale = 12)[1],
      SortinoRatio = SortinoRatio(Ra, Rf = rf / 12, scale = 12)[1],
      MaxDrawdown = maxDrawdown(Ra)[1],
      VaR_95 = VaR(Ra, p = 0.95, method = "historical")[1],
      CVaR_95 = CVaR(Ra, p = 0.95, method = "historical")[1],
      Beta = CAPM.beta(Ra, benchmark_returns)[1]
    )
  })

  bind_rows(metrics) %>% mutate(across(where(is.numeric), round, 4))
}
evaluate_all_metrics(perf_long)




















