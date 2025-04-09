sharpe_optim <- function(path_to_csv = "data/monthly_log_returns.csv", rf_annual = 0.044) {
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
  sigma <- cov.shrink(returns_mat) * 12  # annualiseret kovariansmatrix

  # 2. Objektivfunktion: max Sharpe ratio (vi minimerer negativ Sharpe)
  sharpe_obj <- function(w) {
    port_ret <- sum(w * mu_annual)
    port_vol <- sqrt(t(w) %*% sigma %*% w)
    -(port_ret - rf_annual) / port_vol
  }

  # 3. Optimér med begrænsninger: vægte ≥ 0 og sum = 1
  dres <- optim(
    par = rep(1 / length(mu_annual), length(mu_annual)),
    fn = sharpe_obj,
    method = "L-BFGS-B",
    lower = rep(0, length(mu_annual)),
    upper = rep(1, length(mu_annual)),
    control = list(fnscale = 1)
  )

  w_opt <- dres$par / sum(dres$par)  # normaliser vægte
  port_ret <- sum(w_opt * mu_annual)
  port_vol <- sqrt(t(w_opt) %*% sigma %*% w_opt)
  sharpe <- (port_ret - rf_annual) / port_vol

  # 4. Returnér som tibble
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



sharpe_optim_cardinality <- function(
  path_to_csv  = "data/monthly_log_returns.csv",
  rf_annual    = 0.044,
  max_assets   = 10,
  search_size  = 2000
) {
  # Pakker
  library(tidyverse)
  library(xts)
  library(PortfolioAnalytics)
  library(ROI)
  library(ROI.plugin.glpk)       # Til lineære constraints
  # Hvis du får “Fejl i gmv_opt... plugin...”, kan du også prøve:
  # install.packages("ROI.plugin.quadprog")
  # library(ROI.plugin.quadprog)

  # 1) Læs data til xts
  returns <- read_csv(path_to_csv, show_col_types = FALSE) %>%
    rename(Date = 1) %>%
    mutate(Date = as.Date(Date)) %>%
    drop_na()

  returns_xts <- xts::xts(returns[, -1], order.by = returns$Date)

  # 2) Definér portfolio-spec med cardinality constraint
  asset_names <- colnames(returns_xts)
  pspec <- portfolio.spec(assets = asset_names) %>%
    add.constraint(type = "full_investment") %>%
    add.constraint(type = "long_only") %>%
    add.constraint(type = "position_limit", max_pos = max_assets)

  # 3) Definér Sharpe-objektiv som custom-funktion.
  #    Bemærk at solveren i PortfolioAnalytics minimerer "risk"-objektivet,
  #    så vi returnerer -Sharpe.
  sharpe_custom <- function(R, weights) {
    port_rets <- as.numeric(R %*% weights)
    mu_m      <- mean(port_rets)
    sd_m      <- sd(port_rets)
    # Årligt
    mu_annual <- mu_m  * 12
    sd_annual <- sd_m  * sqrt(12)
    # Sharpe
    shr <- (mu_annual - rf_annual) / sd_annual
    # Retuner NEGATIV Sharpe => minimér => max Sharpe
    return(-shr)
  }

  pspec <- add.objective(
    portfolio  = pspec,
    type       = "risk",   # “risk”-type => minimer
    name       = "CustomSharpe",
    arguments  = list(FUN = sharpe_custom)
  )

  # 4) Kør heuristisk optimering
  opt_result <- optimize.portfolio(
    R                = returns_xts,
    portfolio        = pspec,
    optimize_method  = "DE",       # Differential Evolution
    search_size      = search_size,
    trace            = FALSE
  )

  # 5) Udpak vægte
  w_opt <- extractWeights(opt_result)
  if (is.null(w_opt)) {
    stop("Solveren fandt ingen brugbar løsning. Prøv større search_size, anden metode, eller check constraints/data.")
  }

  # 6) Beregn Sharpe i den fundne løsning
  port_returns <- as.numeric(returns_xts %*% w_opt)
  mu_m         <- mean(port_returns)
  sd_m         <- sd(port_returns)
  mu_annual    <- mu_m  * 12
  sd_annual    <- sd_m  * sqrt(12)
  sharpe_val   <- (mu_annual - rf_annual) / sd_annual

  # 7) Returnér tibble med de aktiver, der faktisk indgår (Weight > 0)
  #    + portefølje-statistik
  tibble(
    Ticker              = asset_names,
    Weight              = round(w_opt, 4)
  ) %>%
    filter(Weight > 0) %>%
    arrange(desc(Weight)) %>%
    mutate(
      PortfolioReturn     = round(mu_annual, 4),
      PortfolioVolatility = round(sd_annual, 4),
      SharpeRatio         = round(sharpe_val, 4),
      Method = paste0("Sharpe w. cardinality ≤ ", max_assets)
    )
}


