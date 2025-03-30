# ==============================================================================
# 1) Pakker og kildefiler
# ==============================================================================
library(tidyverse)
library(quantmod)
library(corpcor)
library(xts)
library(lubridate)

# Sourcing af egne scripts
source("cvar.R")       # Her ligger din mean_cvar_optim()-funktion
source("sharpe_opt.R") # Her ligger din sharpe_optim()-funktion
source("plot_portfolio_vs_benchmark.R")

# ==============================================================================
# 2) Funktioner til performance-sammenligning
# ==============================================================================
summarize_performance <- function(weights, returns_mat, rf_annual = 0.044) {
  mu_monthly <- colMeans(returns_mat)
  mu_annual  <- mu_monthly * 12
  sigma      <- corpcor::cov.shrink(returns_mat) * 12

  port_ret   <- sum(weights * mu_annual)
  port_vol   <- sqrt(t(weights) %*% sigma %*% weights)
  sharpe     <- (port_ret - rf_annual) / port_vol

  tibble(
    `Årligt afkast`     = round(port_ret, 4),
    `Årlig volatilitet` = round(port_vol, 4),
    `Sharpe Ratio`      = round(sharpe, 4)
  )
}

# Funktion til at beregne Beta mod benchmark
estimate_beta <- function(weights, returns_mat, benchmark_name) {
  port_returns      <- as.numeric(returns_mat %*% weights)
  benchmark_returns <- returns_mat[, benchmark_name]

  model      <- lm(port_returns ~ benchmark_returns)
  beta       <- coef(model)[2]
  r_squared  <- summary(model)$r.squared

  tibble(
    Beta     = round(beta, 4),
    R_squared = round(r_squared, 4)
  )
}

# Funktion til at estimere forventet indtjening
indtjeningscalc <- function(multiplier = 1, profit_pct = 1, rf = 0.044, perf_table) {
  cap <- 100000 * multiplier

  perf_table %>%
    mutate(
      Forventet_afkast = `Årligt afkast`,
      SD               = `Årlig volatilitet`,
      Mean_Profit      = pmax(cap * Forventet_afkast * profit_pct, 0),
      Lower_CI         = pmax(cap * (Forventet_afkast - 1.96 * SD) * profit_pct, 0),
      Upper_CI         = cap * (Forventet_afkast + 1.96 * SD) * profit_pct
    ) %>%
    select(Portefølje, Mean_Profit, Lower_CI, Upper_CI)
}

# ==============================================================================
# 3) Indlæs data og opdater med VUSA (hvis muligt)
# ==============================================================================
# Bemærk: Sørg for at filen 'data_udvalgt.csv' allerede eksisterer i mappen 'data'

# 3.1 Læs eksisterende data
returns_raw <- read_csv("data/data_udvalgt.csv") %>%
  rename(Date = 1) %>%
  mutate(Date = as.Date(Date)) %>%
  drop_na()

# 3.2 Konverter til xts
returns_xts <- xts(
  returns_raw %>% select(-Date),
  order.by = returns_raw$Date
)

# 3.3 Funktion til at hente VUSA
get_vusa <- function(from = "2021-01-01") {
  tryCatch({
    vusa_px  <- Ad(getSymbols("VUSA.AS", from = from, src = "yahoo", auto.assign = FALSE))
    vusa_ret <- monthlyReturn(vusa_px, type = "log")
    colnames(vusa_ret) <- "VUSA.AS"
    return(vusa_ret)
  }, error = function(e) {
    message("Kunne ikke hente VUSA.AS: ", e$message)
    return(NULL)
  })
}

# 3.4 Hent VUSA og merge ind i datasættet (hvis muligt)
vusa_ret <- get_vusa()
if (!is.null(vusa_ret)) {
  combined_returns <- merge(returns_xts, vusa_ret, join = "inner")
  write.zoo(combined_returns, file = "data/data_udvalgt.csv", sep = ",")
  message("✅ VUSA.AS tilføjet til data_udvalgt.csv")
} else {
  warning("❌ VUSA.AS blev ikke tilføjet.")
}

# 3.5 Læs data på NY efter at VUSA er tilføjet (ellers har vi stadig kun de gamle kolonner)
returns <- read_csv("data/data_udvalgt.csv") %>%
  rename(Date = 1) %>%
  mutate(Date = as.Date(Date)) %>%
  drop_na()

# 3.6 Lav nu matrix
returns_mat <- returns %>%
  select(-Date) %>%
  as.matrix()

# Sæt benchmark til "VUSA.AS", hvis ønsket
benchmark_name <- "VUSA.AS"

# Opret vægte til benchmark (1 for kolonne VUSA.AS, 0 for alle andre)
benchmark_weights <- setNames(
  as.numeric(colnames(returns_mat) == benchmark_name),
  colnames(returns_mat)
)

# ==============================================================================
# 4) Kør Sharpe- og CVaR-optimering
# ==============================================================================
cvar_result   <- mean_cvar_optim()
sharpe_result <- sharpe_optim()

# Du kan se vægtene direkte ved at printe
cvar_result
sharpe_result

# ==============================================================================
# 5) Plot porteføljer kontra benchmark
# ==============================================================================
plot_portfolios_vs_benchmark(
  sharpe_result,
  cvar_result,
  benchmark_name = benchmark_name
)

# ==============================================================================
# 6) Byg en performance-oversigtstabel
# ==============================================================================
perf_table <- bind_rows(
  Sharpe     = summarize_performance(sharpe_result$Weight, returns_mat),
  CVaR       = summarize_performance(cvar_result$Weight, returns_mat),
  Benchmark  = summarize_performance(benchmark_weights, returns_mat),
  .id = "Portefølje"
)

# Vis performance
perf_table

# ==============================================================================
# 7) Indtjeningsberegning
# ==============================================================================
indtjening <- indtjeningscalc(
  multiplier = 10,    # F.eks. investerer man 1 mio i stedet for 100k
  profit_pct = 0.2,   # Måske tager man kun 20% i realiseret profit
  perf_table = perf_table
)

indtjening

# ==============================================================================
# 8) Estimér Beta mod benchmark
# ==============================================================================
beta_table <- bind_rows(
  Sharpe    = estimate_beta(sharpe_result$Weight, returns_mat, benchmark_name),
  CVaR      = estimate_beta(cvar_result$Weight, returns_mat, benchmark_name),
  Benchmark = tibble(Beta = 1, R_squared = 1),
  .id       = "Portefølje"
)

beta_table

# ==============================================================================
# 9) Slå performance og beta sammen
# ==============================================================================
perf_summary <- perf_table %>%
  left_join(beta_table, by = "Portefølje")

# Udskriv den samlede performance-oversigt
perf_summary
