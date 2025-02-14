# R/plot_portfolio.R

plot_portfolio <- function(cumulative_returns, weights) {
  # Beregn porteføljeafkast
  portfolio_returns <- rowSums(cumulative_returns[, -ncol(cumulative_returns)] * weights[-ncol(cumulative_returns)])

  # Opret plotdata (kun portefølje og benchmark)
  plot_data <- data.frame(
    Date = index(cumulative_returns),
    Portfolio = portfolio_returns,
    Benchmark = cumulative_returns$Benchmark
  ) %>%
    pivot_longer(cols = -Date, names_to = "Asset", values_to = "CumulativeReturn")

  # Plot
  ggplot(plot_data, aes(x = Date, y = CumulativeReturn, color = Asset)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("Portfolio" = "#00FF00", "Benchmark" = "#FF0000")) +
    labs(
      title = "Portefølje vs Benchmark Kumulativt Afkast",
      x = "Dato",
      y = "Kumulativt Afkast (%)"
    ) +
    theme_minimal()
}

# Funktion til at vise vægte
display_weights <- function(weights, etf_symbols) {
  data.frame(
    Asset = c(etf_symbols, "Benchmark"),
    Weight = paste0(round(weights * 100, 2), "%")  # Viser vægte i %
  )
}
