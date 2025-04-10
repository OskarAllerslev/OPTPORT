
library(quantmod)
library(tidyverse)
library(lubridate)

#
# tickers <- c("EUNL.DE", "EXXT.DE", "ERNE.DE", "XD9U.DE", "IQQH.DE",
#              "DBX1.DE", "IS3N.DE", "SXR8.DE", "XDUK.DE", "EXS1.DE",
#              "SPYW.DE", "CSEM.DE", "EUNS.DE", "CSPX.DE", "SXRJ.DE")
ftse100_tickers <- c(
  "AZN.L", "HSBA.L", "BP.L", "GSK.L", "DGE.L",
  "ULVR.L", "BATS.L", "RIO.L", "GLEN.L", "SHEL.L",
  "VOD.L", "RDSA.L", "RDSB.L", "BARC.L", "LLOY.L",
  "NG.L", "REL.L", "PR"
)

dax40_tickers <- c(
  "ADS.DE", "AIR.DE", "ALV.DE", "BAS.DE", "BAYN.DE",
  "BEI.DE", "BMW.DE", "CON.DE", "1COV.DE", "DAI.DE",
  "DHER.DE", "DBK.DE", "DB1.DE", "DPW.DE", "DTE.DE",
  "DWNI.DE", "EOAN.DE", "FRE.DE", "FME.DE", "HEI.DE",
  "HEN3.DE", "IFX.DE", "LIN.DE", "MRK.DE", "MTX.DE",
  "MUV2.DE", "RWE.DE", "SAP.DE", "SIE.DE", "SY1.DE",
  "VOW3.DE", "VNA.DE", "WDI.DE", "ZAL.DE", "HFG.DE",
  "LEG.DE", "SHL.DE", "SRT3.DE", "ENR.DE", "PUM.DE"
)

nasdaq100_tickers <- c(
  "AAPL", "MSFT", "AMZN", "GOOGL", "META",
  "NVDA", "TSLA", "PYPL", "ADBE", "CMCSA",
  "NFLX", "INTC", "PEP", "CSCO", "AVGO",
  "COST", "TMUS", "TXN", "QCOM", "CHTR",
  "AMGN", "SBUX", "INTU", "ISRG", "MDLZ",
  "ADP", "MU", "BKNG", "GILD", "LRCX",
  "FISV", "ATVI", "ADSK", "CSX", "ILMN",
  "MRNA", "MELI", "KHC", "DXCM", "CDNS",
  "KLAC", "LULU", "ORLY", "PAYX", "XEL",
  "IDXX", "WBA", "VRSK", "MAR", "MTCH",
  "CTAS", "PCAR", "EXC", "MNST", "EBAY",
  "ROST", "SIRI", "VRSN", "NTES", "FAST",
  "BIIB", "ALGN", "SWKS", "DLTR", "ANSS",
  "WDAY", "TTD", "SNPS", "OKTA", "TEAM",
  "ZS", "DOCU", "SGEN", "BIDU", "JD",
  "PDD", "MCHP", "CDW", "SPLK", "CTSH",
  "NTAP", "CHKP", "FOXA", "FOX", "VRSK",
  "LBTYA", "LBTYK", "LILA", "LILAK", "CERN",
  "PTON", "BMRN", "INCY", "MXIM", "UAL",
  "EXPE", "JBHT", "WDC", "TTWO", "NTES"
)

sp500_tickers <- c(
  "AAPL", "MSFT", "AMZN", "GOOGL", "META",
  "BRK.B", "NVDA", "TSLA", "JPM", "JNJ",
  "V", "PG", "UNH", "HD", "DIS",
  "PYPL", "MA", "INTC", "NFLX", "ADBE",
  "CMCSA", "PFE", "KO", "PEP", "T",
  "XOM", "CSCO", "ABT", "CVX", "MRK",
  "NKE", "WFC", "MCD", "IBM", "BA",
  "ORCL", "GE", "HON", "COST", "SBUX",
  "MDT", "AMGN", "CAT", "MMM", "GS",
  "ISRG", "BLK", "BKNG", "ZTS", "TMO",
  "FIS", "CI", "GILD", "LMT", "SYK",
  "ADP", "CB", "USB", "BMY", "SCHW",
  "SPGI", "PLD", "MO", "AXP", "C",
  "TJX", "DUK", "SO", "BDX", "CCI",
  "CL", "NSC", "ICE", "ITW", "PNC",
  "ETN", "AON", "WM", "CSX", "EW",
  "ROP", "TFC", "EMR", "HUM", "DHR",
  "APD", "ECL", "FISV", "MAR", "AIG",
  "PSA", "MCO", "ADSK", "AEP", "D",
  "KMB", "MET", "A", "IDXX", "AZO",
  "TRV", "SHW", "CTAS", "ANSS", "VRSK"
)

tickers_i <- c(
  "EUNL.DE", "EXXT.DE", "ERNE.DE", "XD9U.DE", "IQQH.DE", "DBX1.DE", "IS3N.DE", "SXR8.DE", "XDUK.DE", "EXS1.DE",
  "SPYW.DE", "CSEM.DE", "EUNS.DE", "CSPX.DE", "SXRJ.DE", "VUSA.DE", "VUAA.DE", "VEUR.DE", "VGVE.DE", "VGEA.DE",
  "EUNM.DE", "EUNH.DE", "IBCI.DE", "XDWD.DE", "XMME.DE", "XDJP.DE", "XDEX.DE", "XDEM.DE", "XCS6.DE", "XCS5.DE",
  "CEMB.DE", "CEMG.DE", "CEUG.DE", "CS51.DE", "XESC.DE", "XMML.DE", "XDWT.DE", "XUEM.DE", "XDUS.DE", "XD9A.DE",
  "XD7S.DE", "XDWT.DE", "ISPY.DE", "SPPX.DE", "SPXS.DE", "XD5E.DE", "XDW0.DE", "XD9W.DE", "XDWE.DE", "XD9Z.DE",
  "XD6U.DE", "XD6E.DE", "XD9G.DE", "XD9F.DE", "XD9D.DE", "XD9C.DE", "XDUK.DE", "XDER.DE", "XDBE.DE", "XDDF.DE",
  "EQQQ.DE", "IQQW.DE", "IQQD.DE", "IQQE.DE", "IQQA.DE", "IQQH.DE", "IQQJ.DE", "IQQK.DE", "IQQT.DE", "IQQN.DE",
  "SXR1.DE", "SXR2.DE", "SXR3.DE", "SXR4.DE", "SXR5.DE", "SXR6.DE", "SXR7.DE", "SXR8.DE", "SXR9.DE", "SXRV.DE",
  "SDJP.DE", "SDUS.DE", "SUSM.DE", "SUSL.DE", "SEMA.DE", "SEMG.DE", "SEML.DE", "SEMR.DE", "SEMT.DE", "SEMX.DE",
  "ZPRV.DE", "ZPRS.DE", "ZPRA.DE", "ZPRB.DE", "ZPRC.DE", "ZPRD.DE", "ZPRE.DE", "ZPRF.DE", "ZPRG.DE", "ZPRH.DE",
  "XS6R.DE", "XS6S.DE", "XS6T.DE", "XS6U.DE", "XS6V.DE", "XS6W.DE", "XS6X.DE", "XS6Y.DE", "XS6Z.DE", "XS70.DE",
  "XD6F.DE", "XD6G.DE", "XD6H.DE", "XD6J.DE", "XD6K.DE", "XD6L.DE", "XD6M.DE", "XD6N.DE", "XD6O.DE", "XD6P.DE"
)


tickers <- c(ftse100_tickers,dax40_tickers,nasdaq100_tickers ,sp500_tickers, tickers_i )

get_prices <- function(ticker, from = "2021-01-01") {
  tryCatch({
    raw <- getSymbols(ticker, src = "yahoo", from = from, auto.assign = FALSE)
    px <- Ad(raw)
    colnames(px) <- "Price"  # Ensartet navn
    return(px)
  }, error = function(e) {
    message(paste("Fejl ved:", ticker, ":", e$message))
    return(NULL)
  })
}


# get_prices("AAPL", from = "2020-01-01")

ret_list <- list()


for (t in tickers) {
  cat("🔄 Behandler:", t, "\n")
  prices <- get_prices(t)

  if (!is.null(prices)) {
    monthly <- tryCatch({
      monthlyReturn(prices, type = "log")
    }, error = function(e) {
      message(paste("❌ monthlyReturn fejl ved:", t))
      return(NULL)
    })

    # Kun behold, hvis nok data og ingen fejl
    if (!is.null(monthly) && nrow(monthly) >= 36) {
      colnames(monthly) <- t
      ret_list[[t]] <- monthly
      cat("✅ Tilføjet:", t, "med", nrow(monthly), "observationer\n")
    } else {
      cat("⚠️ Ikke nok data for:", t, "\n")
    }
  }
}


returns_df <- do.call(merge, ret_list)
returns_df <- na.omit(returns_df)
write.csv(returns_df, "data/monthly_log_returns.csv")

cat("Task done. No of assets: ", ncol(returns_df), "\n")


