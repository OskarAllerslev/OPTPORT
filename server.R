# ---- server.R ----
library(shiny)
library(xts)
library(PerformanceAnalytics)

shinyServer(function(input, output, session) {

  #
  # 1) Forklarende tekst (UI)
  #
  output$explanation <- renderUI({
    tagList(
      h2("Maximer Sharpe Ratio (uden risikofri rente)"),
      p("Denne app henter historiske data for udvalgte ETF'er og beregner ",
        "en portefølje, der maksimerer Sharpe Ratio (forventet afkast / risiko)."),
      p("Du kan vælge, om short-selling skal være tilladt. ")
    )
  })


  #
  # 2) Hent og beregn historiske data
  #
  data_reactive <- eventReactive(input$goButton, {
    # Brugerens input for år
    from_date <- as.Date(Sys.Date() - (input$lookbackYears * 365))

    all_tickers <- c(etf_tickers, benchmark_ticker)
    prices <- get_data_from_yahoo(
      tickers   = all_tickers,
      from_date = from_date,
      to_date   = Sys.Date()
    )

    if(!is.null(prices)) {
      # Daglige log-afkast
      ret_all <- na.omit(diff(log(prices)))
      return(ret_all)
    } else {
      return(NULL)
    }
  })


  #
  # 3) Sharpe-optimering
  #
  opt_result <- eventReactive(input$goButton, {
    ret_all <- data_reactive()
    req(ret_all)

    # Kun ETF'erne (ekskl. benchmark)
    etf_ret <- ret_all[, etf_tickers, drop=FALSE]

    # Skaler til årligt (log) - groft ved at gange med 252
    R_annual <- etf_ret * 252

    # Find de vægte, der giver højeste Sharpe ratio
    w <- markowitz_max_sharpe(
      R = R_annual,
      short_allowed = input$shortAllowed,
      n_points = 50  # hvor mange "target"-trin vi søger over
    )

    list(weights = w)
  })


  #
  # 4) Resultattabel
  #
  output$resultsTable <- renderTable({
    ret_all <- data_reactive()
    req(ret_all)

    w <- opt_result()$weights
    if(all(is.na(w))) {
      return(data.frame(
        Besked = "Ingen løsning fundet. Tjek om data er valide, eller prøv at tillade short selling."
      ))
    }

    etf_ret  <- ret_all[, etf_tickers, drop=FALSE]
    # Årlige logafkast
    R_annual <- etf_ret * 252

    mu      <- colMeans(R_annual)
    cov_mat <- cov(R_annual)

    # Porteføljens årlige forventede afkast og std
    exp_return_port <- sum(w * mu)
    exp_vol_port    <- sqrt(t(w) %*% cov_mat %*% w)
    sharpe_port     <- exp_return_port / exp_vol_port

    # Benchmark
    bench_ret_log   <- ret_all[, benchmark_ticker, drop=FALSE]
    bench_annual_ret <- mean(bench_ret_log) * 252
    bench_annual_vol <- sqrt(var(bench_ret_log) * 252)
    bench_sharpe    <- bench_annual_ret / bench_annual_vol

    # Tabel over hver ETF
    df_assets <- data.frame(
      Ticker        = etf_tickers,
      VægtPct       = round(w * 100, 2),
      ForvAfkastPct = round(mu * 100, 2)
    )

    # Tilføj portefølje + benchmark
    df_port <- data.frame(
      Ticker        = "Portefølje (Max Sharpe)",
      VægtPct       = "-",
      ForvAfkastPct = round(exp_return_port * 100, 2)
    )
    df_bench <- data.frame(
      Ticker        = paste("Benchmark:", benchmark_ticker),
      VægtPct       = "-",
      ForvAfkastPct = round(bench_annual_ret * 100, 2)
    )

    df_all <- rbind(df_assets, df_port, df_bench)

    # Læg kolonner for standardafvigelse og Sharpe
    df_all$ForvStdPct <- NA
    df_all$Sharpe     <- NA

    # Portefølje
    df_all$ForvStdPct[df_all$Ticker == "Portefølje (Max Sharpe)"] <-
      round(exp_vol_port * 100, 2)

    df_all$Sharpe[df_all$Ticker == "Portefølje (Max Sharpe)"] <-
      round(sharpe_port, 3)

    # Benchmark
    bench_lab <- paste("Benchmark:", benchmark_ticker)
    df_all$ForvStdPct[df_all$Ticker == bench_lab] <-
      round(bench_annual_vol * 100, 2)

    df_all$Sharpe[df_all$Ticker == bench_lab] <-
      round(bench_sharpe, 3)

    df_all
  }, striped = TRUE, spacing = "m", align = "c")


  #
  # 5) Plot af kumulative afkast
  #
  output$plotAll <- renderPlot({
    ret_all <- data_reactive()
    req(ret_all)

    # Portefølje-vægte
    w <- opt_result()$weights
    if(all(is.na(w))) return(NULL)

    # Tag sidste 1 år til plottet
    last_year <- Sys.Date() - 365
    ret_1yr_log <- ret_all[paste0(last_year, "/"), , drop=FALSE]

    if(nrow(ret_1yr_log) < 2) {
      showNotification("For få datapunkter til at plotte.", type="warning")
      return(NULL)
    }

    etf_ret_log   <- ret_1yr_log[, etf_tickers, drop=FALSE]
    bench_ret_log <- ret_1yr_log[, benchmark_ticker, drop=FALSE]

    # -- ETF'erne: lav log -> aritmetisk -> kumuleret % --
    etf_arith <- exp(etf_ret_log) - 1
    etf_cum   <- cumprod(1 + etf_arith) - 1
    etf_cum   <- etf_cum * 100  # i procent

    # -- Porteføljen --
    port_ret_log <- rowSums(etf_ret_log * w)
    port_ret_log <- xts(port_ret_log, order.by=index(etf_ret_log))
    port_arith   <- exp(port_ret_log) - 1
    port_cum     <- cumprod(1 + port_arith) - 1
    port_cum     <- port_cum * 100
    colnames(port_cum) <- "Portefølje"

    # -- Benchmark --
    bench_arith <- exp(bench_ret_log) - 1
    bench_cum   <- cumprod(1 + bench_arith) - 1
    bench_cum   <- bench_cum * 100
    colnames(bench_cum) <- "Benchmark"

    combined_cum <- merge(etf_cum, port_cum, bench_cum)

    # Farver: ETF’er grå, portefølje grøn, benchmark rød
    n_etf  <- ncol(etf_cum)
    mycols <- c(rep("grey70", n_etf), "green", "red")
    mylwd  <- c(rep(1, n_etf), 2, 2)

    chart.TimeSeries(
      combined_cum,
      legend.loc = "topleft",
      main       = "ETF'er (grå), Portefølje (grøn), Benchmark (rød)",
      ylab       = "Kumulativ afkast (%)",
      colorset   = mycols,
      lwd        = mylwd
    )
  })

})
