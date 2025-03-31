###############################################
## backtest_sharpe.R
## --------------------------------------------
##  1) Indlæs data
##  2) Sharpe-optimer rullende
##  3) Sammenlign med VUSA.AS
##  4) PerformanceAnalytics opsummering
###############################################

## Ryd environment (valgfrit)
rm(list=ls())

## Libraries
library(tidyverse)
library(xts)
library(lubridate)
library(PerformanceAnalytics)
library(corpcor)       # til cov.shrink
library(quantmod)      # til getSymbols

###############################################
# TRIN 1: Læs data, sæt datoer
###############################################

# Antag at "data_udvalgt.csv" har 48 månedlige log-afkast-rækker
# (justér hvis du har flere eller færre)
# Vi ignorerer den oprindelige "Date"-kolonne, da den er forkert
# og laver i stedet nye datoer fra 2021-01-01 månedligt.

returns_df <- read_csv("data/data_udvalgt.csv") %>%
  select(-Date) %>%
  mutate(Date = seq.Date(from = as.Date("2021-01-01"),
                         by = "month", length.out = n())) %>%
  relocate(Date)

# Tjek om den nu har 48 (eller dine) rækker:
print(dim(returns_df))    # fx 48 x 24

# Lav matrix og gem datoer
R_mat  <- returns_df %>% select(-Date) %>% as.matrix()
dates  <- returns_df$Date

###############################################
# TRIN 2: Sharpe-optimerende funktion + rullende backtest
###############################################

sharpe_optimize_weights <- function(R_window, rf_annual = 0.044){
  mu_monthly <- colMeans(R_window)
  mu_annual  <- mu_monthly * 12
  sigma      <- corpcor::cov.shrink(R_window) * 12

  sharpe_obj <- function(w) {
    port_ret <- sum(w * mu_annual)
    port_vol <- sqrt(t(w) %*% sigma %*% w)
    # Vi vil maksimere Sharpe => minimere negativ Sharpe
    -(port_ret - rf_annual) / port_vol
  }

  # Optimér vægte med constraints: w i [0,1], sum(w)=1
  dres <- optim(
    par     = rep(1 / length(mu_annual), length(mu_annual)),
    fn      = sharpe_obj,
    method  = "L-BFGS-B",
    lower   = rep(0, length(mu_annual)),
    upper   = rep(1, length(mu_annual)),
    control = list(fnscale = 1)
  )

  # Normalisér vægte hvis summen ikke er præcist 1 pga. numerik
  w_opt <- dres$par / sum(dres$par)
  return(w_opt)
}

run_backtest_sharpe_rolling <- function(R_mat, rf_annual = 0.044,
                                        start_index = 13,
                                        window_size = 12,
                                        rebalance_horizon = 3)
{
  n <- nrow(R_mat)
  port_returns <- rep(NA, n)  # vektor med porteføljeafkast

  current <- start_index
  while ((current + rebalance_horizon - 1) <= n) {

    # Vindue til at optimere vægte
    R_window  <- R_mat[(current - window_size):(current - 1), , drop = FALSE]
    weights   <- sharpe_optimize_weights(R_window, rf_annual)

    # Perioden vi holder vægtene i
    R_forward <- R_mat[current:(current + rebalance_horizon - 1), , drop = FALSE]
    # porteføljeafkast pr. måned
    these_returns <- R_forward %*% weights

    # Gem i vores result-vektor
    port_returns[current:(current + rebalance_horizon - 1)] <- these_returns

    # Gå videre i 3-måneders-skridt
    current <- current + rebalance_horizon
  }

  return(port_returns)
}

###############################################
# TRIN 3: Kør rullende backtest
###############################################

port_ret <- run_backtest_sharpe_rolling(
  R_mat       = R_mat,
  rf_annual   = 0.044,
  start_index = 13,     # Starter i Jan 2022 => de første 12 mdr = 2021
  window_size = 12,     # 12 mdr lookback
  rebalance_horizon = 3 # rebalancering hver 3. måned
)


# Konverter til xts for videre analyse, NA-fjern
port_xts <- xts(port_ret, order.by = dates)
port_xts <- na.omit(port_xts) # udelad start-NA

###############################################
# TRIN 4: Hent og justér benchmark (VUSA.AS)
###############################################

# Hent daglige priser (2021 -> nu)
benchmark_price <- getSymbols("VUSA.AS", src = "yahoo", from = "2021-01-01",
                              auto.assign = FALSE)

# Lav månedlige logafkast
benchmark_monthly <- monthlyReturn(Ad(benchmark_price), type = "log")

# Vi vil matche porteføljens slutdage i hver måned
# => tag port_xts's endpoints
port_xts_eom <- port_xts[endpoints(port_xts, on = "months")]



library(zoo)

# Gør port_xts' indeks til 'as.yearmon' og sæt 'frac=1' => sidste dag i måned
index(port_xts) <- as.Date(as.yearmon(index(port_xts)), frac = 1)

# Merge med join="inner" for at få fælles datoer
combined <- merge(port_xts, benchmark_monthly, join = "inner")
colnames(combined) <- c("Portfolio", "Benchmark")
head(combined)

# Split igen til to xts
portfolio_final  <- combined[,"Portfolio"]
benchmark_final  <- combined[,"Benchmark"]

###############################################
# TRIN 5: PerformanceAnalytics:
#   - Samlet Performance-plot
#   - Rolling Performance
#   - Tabeller med nøgletal
###############################################

# 1) Samlet PerformanceSummary
charts.PerformanceSummary(
  R = combined,
  main = "Rolling Sharpe-optimeret vs. VUSA.AS"
)

# 2) Rolling Performance
# Her f.eks. 12-måneders rullevindue
charts.RollingPerformance(
  R = combined,
  width = 12,
  main = "12-måneders Rolling Performance"
)

# 3) Nøgletal: Sharpe, Return, Vol, m.m.
table.Stats(combined)  # yoy-lignende stats

# 4) Andre tabeller
table.Drawdowns(portfolio_final, top = 2)   # Største drawdowns i porteføljen
table.DownsideRisk(combined)               # Downside risk metrics
table.Correlation(combined$Portfolio, combined$Benchmark)                # Sammenhæng

# 5) Probability of Outperformance?
#    (Denne funktion findes ikke i base PerformanceAnalytics,
#     men i StrategyPerformance-pakken, i "table.ProbOutPerformance()")
#    Her et eksempel, hvis du har StrategyPerformance installeret:
# library(StrategyPerformance)
# table.ProbOutPerformance(
#   R = portfolio_final,
#   Rb = benchmark_final,
#   period_lengths = c(1, 3, 6, 12)
# )

###############################################
# SLUT
###############################################
