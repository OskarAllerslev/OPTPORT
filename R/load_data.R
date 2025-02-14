# R/load_data.R

load_data <- function() {
  # Definér ETF-symboler og benchmark
  etf_symbols <- c("SXR8.DE", "EXH1.DE", "IQQ6.DE", "CSPX.L", "EUNL.DE", "ISF.L", "SMEA.MI", "IBCX.MI", "IQQH.DE")
  benchmark_symbol <- "^OMXC25"

  # Hent ETF-data med ensartet tidsramme
  start_date <- Sys.Date() - 365
  end_date <- Sys.Date()

  etf_data <- lapply(etf_symbols, function(symbol) {
    data <- getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    data <- na.approx(Ad(data))  # Interpolér manglende værdier
    colnames(data) <- symbol
    return(data)
  })

  # Flet ETF-data
  etf_data <- do.call(merge, etf_data)

  # Hent benchmark-data og interpolér
  benchmark_data <- getSymbols(benchmark_symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  benchmark_data <- na.approx(Ad(benchmark_data))
  colnames(benchmark_data) <- "Benchmark"

  # Flet data (kun fælles dage)
  all_data <- merge(etf_data, benchmark_data, join = "inner") %>% na.omit()

  return(all_data)
}
