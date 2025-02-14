# R/calculate_portfolio.R

calculate_portfolio <- function(data, target_return) {
  library(quadprog)

  # Beregn daglige afkast
  returns <- ROC(data, type = "discrete") %>% na.omit()

  # Middelafkast og kovariansmatrix
  mean_returns <- colMeans(returns)
  cov_matrix <- cov(returns)

  # Antal aktiver (ekskl. benchmark)
  n_assets <- ncol(data) - 1

  # Begrænsninger: sum(w) = 1, w >= 0, target_return
  Dmat <- 2 * cov_matrix[1:n_assets, 1:n_assets]
  dvec <- rep(0, n_assets)
  Amat <- cbind(
    rep(1, n_assets),    # sum(w) = 1
    mean_returns[1:n_assets],  # target_return
    diag(n_assets)       # w >= 0
  )
  bvec <- c(1, target_return, rep(0, n_assets))
  meq <- 2  # Antal lighedsbegrænsninger

  # Løs optimering
  result <- solve.QP(Dmat, dvec, Amat, bvec, meq = meq)

  # Ekstraher vægte og normaliser
  weights <- pmax(result$solution, 0)      # Fjern negative vægte
  weights <- weights / sum(weights)        # Normaliser til sum = 1
  weights <- c(weights, 0)                # Tilføj benchmark-vægt

  # Beregn porteføljemål
  portfolio_return <- sum(weights * mean_returns)
  portfolio_risk <- sqrt(t(weights) %*% cov_matrix %*% weights)

  # Benchmark-mål
  benchmark_return <- mean_returns["Benchmark"]
  benchmark_risk <- sqrt(cov_matrix["Benchmark", "Benchmark"])

  # Opret output
  list(
    weights = round(weights, 4),
    metrics = data.frame(
      Metric = c("Return", "Risk (Std Dev)", "Sharpe Ratio"),
      Portfolio = c(portfolio_return, portfolio_risk, portfolio_return / portfolio_risk),
      Benchmark = c(benchmark_return, benchmark_risk, benchmark_return / benchmark_risk)
    ),
    cumulative_returns = cumprod(1 + returns) - 1
  )
}
