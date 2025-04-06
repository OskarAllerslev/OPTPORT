
library(tidyverse)
library(lubridate)
library(corpcor)
library(xts)
library(ggplot2)


returns <- read_csv("data/monthly_log_returns.csv") %>%
  dplyr::rename(Date = 1) %>%
  dplyr::mutate(Date = as.Date(Date))


returns_matrix <- returns %>%
  dplyr::select(-Date) %>%
  as.matrix()

cov_shrinked <- corpcor::cov.shrink(returns_matrix)
# Sørg for at navngive rækker og kolonner
colnames(cov_shrinked) <- colnames(returns_matrix)
rownames(cov_shrinked) <- colnames(returns_matrix)

# Gem som CSV
write.csv(cov_shrinked, "data/shrinked_covariance.csv", row.names = TRUE)

# Gem rækkefølgen af tickers (bruges til verifikation senere)
write_csv(tibble(Ticker = colnames(returns_matrix)), "data/shrinked_covariance_tickers.csv")

cat("✅ Shrinked kovariansmatrix gemt som CSV med rækker og kolonnenavne.\n")


# returns_long <- returns %>%
#   tidyr::pivot_longer(-Date, names_to = "Ticker", values_to = "Return")
#
# volatilitet <- returns_long %>%
#   dplyr::group_by(Ticker) %>%
#   dplyr::summarise(vol = sd(Return, na.rm = TRUE) * sqrt(12))
#
#
#
# cor_mat <- returns %>%
#   dplyr::select(-Date) %>%
#   stats::cor(use = "pairwise.complete.obs")
#
# start_ticker <- volatilitet %>%
#   dplyr::arrange(vol) %>%
#   dplyr::pull(Ticker) %>%
#   .[1]
# selected <- start_ticker
# #
# # # greedy udvælgelse
# # while (length(selected) < 30) {
# #   candidates <- setdiff(colnames(cor_mat), selected)
# #
# #   avg_corr <- sapply(candidates, function(c) {
# #     mean(abs(cor_mat[c, selected]), na.rm = TRUE)
# #   })
# #
# #   next_ticker <- names(sort(avg_corr))[1]
# #   selected <- c(selected, next_ticker)
# # }
#
#
#
#
# volatilitet %>%
#   dplyr::filter( Ticker %in% selected) %>%
#   ggplot2::ggplot(ggplot2::aes(x = reorder(Ticker, vol), y = vol)) +
#   ggplot2::geom_col(fill = "steelblue") +
#   coord_flip()
#
# returns_selected <- returns %>%
#   select(Date, all_of(selected))
#
# write_csv(returns_selected, "data/data_udvalgt.csv")
# cat("data færdigfiltreret")
