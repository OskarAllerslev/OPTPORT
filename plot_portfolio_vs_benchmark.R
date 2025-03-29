plot_portfolios_vs_benchmark <- function(sharpe_result, cvar_result, benchmark_name = "EQQQ.DE") {
  library(tidyverse)
  library(scales)

  # 1. Indlæs og forbered data
  returns_xts <- read_csv("data/data_udvalgt.csv") %>%
    rename(Date = 1) %>%
    mutate(Date = as.Date(Date)) %>%
    drop_na()

  dates <- returns_xts$Date
  returns_mat <- returns_xts %>% select(-Date) %>% as.matrix()

  # 2. Beregn porteføljeafkast
  w_sharpe <- sharpe_result$Weight
  w_cvar <- cvar_result$Weight

  names(w_sharpe) <- sharpe_result$Ticker
  names(w_cvar) <- cvar_result$Ticker

  port_returns_sharpe <- as.numeric(returns_mat %*% w_sharpe)
  port_returns_cvar <- as.numeric(returns_mat %*% w_cvar)

  cum_sharpe <- 100 * exp(cumsum(port_returns_sharpe))
  cum_cvar <- 100 * exp(cumsum(port_returns_cvar))

  # 3. Benchmark
  benchmark_returns <- returns_mat[, benchmark_name]
  cum_benchmark <- 100 * exp(cumsum(benchmark_returns))

  # 4. Fjern benchmark fra aktivunivers
  asset_names <- setdiff(colnames(returns_mat), benchmark_name)
  cum_assets <- apply(returns_mat[, asset_names], 2, function(x) 100 * exp(cumsum(x)))

  # 5. Saml alt i et plot-dataframe
  performance_df <- as_tibble(cum_assets)
  performance_df$Date <- dates
  performance_df <- performance_df %>%
    mutate(Sharpe = cum_sharpe,
           CVaR = cum_cvar,
           Benchmark = cum_benchmark)

  # 6. Omform til long format
  performance_long <- performance_df %>%
    pivot_longer(-Date, names_to = "Asset", values_to = "Value") %>%
    mutate(Type = case_when(
      Asset == "Sharpe" ~ "Sharpe",
      Asset == "CVaR" ~ "CVaR",
      Asset == "Benchmark" ~ "Benchmark",
      TRUE ~ "Asset"
    ))

  # 7. Plot
  ggplot(performance_long, aes(x = Date, y = Value, color = Type, group = Asset)) +
    geom_line(data = performance_long %>% filter(Type == "Asset"),
              size = 0.5, alpha = 0.6) +
    geom_line(data = performance_long %>% filter(Type != "Asset"),
              size = 1.2) +
    scale_color_manual(
      values = c(
        "Sharpe" = "blue",
        "CVaR" = "green4",
        "Benchmark" = "red",
        "Asset" = "gray"
      ),
      labels = c(
        "Sharpe" = "Sharpe-optimeret",
        "CVaR" = "CVaR-optimeret",
        "Benchmark" = "Benchmark"
      )
    ) +
    labs(
      title = "Sammenligning af Porteføljer: Sharpe vs CVaR vs Benchmark",
      subtitle = "Alle aktiver (grå), Sharpe-optimeret (blå), CVaR-optimeret (grøn), Benchmark (rød)",
      y = "Værdi (Start = 100 kr)",
      x = NULL,
      color = "Portefølje-type"
    ) +
    scale_y_continuous(labels = scales::dollar_format(suffix = " kr", prefix = "")) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, margin = margin(b = 10))
    )

}
