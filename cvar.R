mean_cvar_optim <- function(path_to_csv = "data/data_udvalgt.csv", p_cvar = 0.95) {
  library(tidyverse)
  library(xts)
  library(corpcor)
  library(PortfolioAnalytics)
  library(ROI)
  library(ROI.plugin.glpk)

  # 1. Indlæs data
  returns <- read_csv(path_to_csv) %>%
    rename(Date = 1) %>%
    mutate(Date = as.Date(Date))

  returns_xts <- xts::xts(returns %>% select(-Date), order.by = returns$Date)
  returns_xts <- na.omit(returns_xts)

  # 2. Portefølje-specifikation og constraints
  assets <- colnames(returns_xts)
  port_spec <- portfolio.spec(assets = assets) %>%
    add.constraint(type = "full_investment") %>%
    add.constraint(type = "long_only") %>%
    add.objective(type = "return", name = "mean") %>%
    add.objective(type = "risk", name = "CVaR", arguments = list(p = p_cvar))

  # 3. Optimer porteføljen
  opt_result <- optimize.portfolio(R = returns_xts, portfolio = port_spec,
                                   optimize_method = "ROI", trace = FALSE)

  weights <- extractWeights(opt_result)

  # 4. Beregn performance
  mu_monthly <- colMeans(returns_xts)
  mu_annual <- mu_monthly * 12
  sigma <- corpcor::cov.shrink(returns_xts) * 12

  port_ret <- sum(weights * mu_annual)
  port_vol <- sqrt(t(weights) %*% sigma %*% weights)

  # 5. Returnér resultat som tibble
  tibble(
    Ticker = names(weights),
    Weight = round(as.numeric(weights), 4),
    AnnualReturn = round(mu_annual[names(weights)], 4),
    PortfolioReturn = round(port_ret, 4),
    PortfolioVolatility = round(port_vol, 4),
    Method = paste0("Mean-CVaR (", p_cvar * 100, "%)")
  )
}




# plots -------------------------------------------------------------------


library(scales)


# 1. Indlæs og forbered
returns_xts <- read_csv("data/data_udvalgt.csv") %>%
  rename(Date = 1) %>%
  mutate(Date = as.Date(Date)) %>%
  drop_na()

dates <- returns_xts$Date
returns_mat <- returns_xts %>% select(-Date) %>% as.matrix()

# 2. Beregn portefølje og benchmark
port_returns <- as.numeric(returns_mat %*% w_cvar)
cum_port <- 100 * exp(cumsum(port_returns))

benchmark_name <- "EQQQ.DE"
benchmark_returns <- returns_mat[, benchmark_name]
cum_benchmark <- 100 * exp(cumsum(benchmark_returns))

# 3. Fjern benchmark fra asset-matrix inden plot
asset_names <- setdiff(colnames(returns_mat), benchmark_name)
cum_assets <- apply(returns_mat[, asset_names], 2, function(x) 100 * exp(cumsum(x)))

# 4. Saml alt i ét plot-dataframe
performance_df <- as_tibble(cum_assets)
performance_df$Date <- dates

# Tilføj portefølje og benchmark
performance_df <- performance_df %>%
  mutate(Portfolio = cum_port,
         Benchmark = cum_benchmark)

# 5. Omform til long format
performance_long <- performance_df %>%
  pivot_longer(-Date, names_to = "Asset", values_to = "Value") %>%
  mutate(Type = case_when(
    Asset == "Portfolio" ~ "Portfolio",
    Asset == "Benchmark" ~ "Benchmark",
    TRUE ~ "Asset"
  ))

# 6. Plot med farver og lagorden
ggplot() +
  geom_line(
    data = performance_long %>% filter(Type == "Asset"),
    aes(x = Date, y = Value, group = Asset),
    color = "gray", size = 0.5, alpha = 0.7
  ) +
  geom_line(
    data = performance_long %>% filter(Type == "Benchmark"),
    aes(x = Date, y = Value),
    color = "red", size = 1.2
  ) +
  geom_line(
    data = performance_long %>% filter(Type == "Portfolio"),
    aes(x = Date, y = Value),
    color = "green", size = 1.2
  ) +
  labs(title = "📊 Portefølje vs Benchmark vs Aktiver",
       y = "Værdi (Start = 100 kr)", x = NULL) +
  scale_y_continuous(labels = dollar_format(suffix = " kr", prefix = "")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))










