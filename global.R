# ---- global.R ----
library(shiny)
library(quantmod)
library(PerformanceAnalytics)
library(quadprog)

# Vores ETF-liste
etf_tickers <- c(
  "CSPX.L",   # iShares Core S&P 500 UCITS ETF (London)
  "IWDA.AS",  # iShares Core MSCI World UCITS ETF (Amsterdam)
  "EMIM.L",   # iShares Core MSCI Emerging Markets IMI UCITS ETF (London)
  "AGGG.L",   # iShares Core Global Aggregate Bond UCITS ETF (London)
  "SGLN.L",   # iShares Physical Gold ETC (London)
  "EXSA.DE",  # iShares STOXX Europe 600 UCITS ETF (Xetra)
  "EUNL.DE",  # iShares Core MSCI World UCITS ETF (Xetra)
  "IS3N.DE"  # iShares Core MSCI EM IMI UCITS ETF (Xetra)
  # "SPY5.DE",  # SPDR S&P 500 UCITS ETF (Xetra)
  # "VUSA.L",   # Vanguard S&P 500 UCITS ETF (London)
  # "VEUR.L",   # Vanguard FTSE Developed Europe UCITS ETF (London)
  # "XDWD.DE",  # Xtrackers MSCI World UCITS ETF (Xetra)
  # "XMME.DE"   # Xtrackers MSCI Emerging Markets UCITS ETF (Xetra)
)

# Benchmark (C25) - her en ETF-variant
benchmark_ticker <- "XACTC25.CO"

# Funktion til at hente kursdata fra Yahoo
# Returnerer et xts-objekt med adjusted pris for hver ticker
get_data_from_yahoo <- function(tickers, from_date, to_date = Sys.Date()) {

  # Sikr, at from_date/to_date er Date-objekter
  from_date <- as.Date(from_date, origin = "1970-01-01")
  to_date   <- as.Date(to_date,   origin = "1970-01-01")

  all_prices <- list()
  for(t in tickers) {
    cat("Henter data for:", t, "fra:", from_date, "til:", to_date, "\n")

    # Hent data med getSymbols
    obj <- tryCatch({
      getSymbols(
        Symbols     = t,
        from        = as.character(from_date),
        to          = as.character(to_date),
        src         = "yahoo",
        auto.assign = FALSE
      )
    }, error = function(e) {
      warning(paste("Kunne ikke hente data for", t, ":", e$message))
      return(NULL)
    })

    if(!is.null(obj)) {
      adj_col <- grep("Adjusted", colnames(obj), value=TRUE)
      if(length(adj_col) == 1) {
        prices <- obj[, adj_col]
        colnames(prices) <- t
        all_prices[[t]] <- prices
      } else {
        warning(paste("Ingen 'Adjusted' kolonne for", t))
      }
    }
  }

  if(length(all_prices) > 0) {
    merged <- do.call(merge, all_prices)
    return(merged)
  } else {
    return(NULL)
  }
}

# Hjælpefunktion til at løse min. varians givet et target-afkast
# (Hvis man vil bruge param. søgning i Sharpe-optimering.)
markowitz_optimization <- function(R, target, short_allowed = FALSE) {

  mu <- colMeans(R)
  cov_mat <- cov(R)
  n <- length(mu)

  # solve.QP løser:
  # min (1/2)*w'Cov*w
  # s.t.  sum(w)=1
  #       mu'w=target
  #       w>=0 hvis short ikke er tilladt
  Dmat <- 2 * cov_mat
  dvec <- rep(0, n)

  Aeq <- rbind(rep(1, n), mu)
  beq <- c(1, target)

  # I solve.QP er alle constraints af typen >=,
  # så for ligehed bruger vi Aeq og -Aeq:
  A <- rbind(Aeq, -Aeq)
  b <- c(beq, -beq)

  # Non-negativity
  if(!short_allowed) {
    A_nonneg <- diag(n)
    b_nonneg <- rep(0, n)
    A <- rbind(A, A_nonneg)
    b <- c(b, b_nonneg)
  }

  sol <- solve.QP(Dmat, dvec, t(A), b, meq=2)
  return(sol$solution)
}

# Funktion, der søger den portefølje, som maksimerer Sharpe Ratio
# (her antager vi ingen risikofri rente, så vi maksimerer mu'w / sqrt(w'Cov w)).
# Løsning: Vi paramétersøger over mulige target-afkast og tager den med højst ratio.
markowitz_max_sharpe <- function(R, short_allowed = FALSE, n_points = 50) {
  mu <- colMeans(R)
  cov_mat <- cov(R)

  # Definér interval for mulige "target"-afkast
  # Her bruges en sekvens fra lidt under min(mu) til lidt over max(mu).
  min_mu <- min(mu)
  max_mu <- max(mu)

  # Hvis max_mu er meget lille eller negativ, fornuftigt at sætte et interval, der omfatter 0
  low_target  <- min(0, 1.1*min_mu)
  high_target <- max(0.01, 1.5*max_mu)

  ret_seq <- seq(low_target, high_target, length.out=n_points)

  best_ratio <- -1e9
  best_w <- rep(NA, length(mu))

  for(target_try in ret_seq) {
    w_try <- tryCatch({
      markowitz_optimization(R, target=target_try, short_allowed=short_allowed)
    }, error=function(e) {
      return(NA)
    })

    if(!any(is.na(w_try))) {
      port_return <- sum(w_try * mu)
      port_vol    <- sqrt(t(w_try) %*% cov_mat %*% w_try)
      # Sharpe ratio (ingen risikofri rente):
      ratio       <- port_return / port_vol

      if(ratio > best_ratio) {
        best_ratio <- ratio
        best_w     <- w_try
      }
    }
  }

  return(best_w)
}
