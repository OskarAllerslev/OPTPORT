
library(quantmod)
library(tidyverse)
library(lubridate)

#
# tickers <- c("EUNL.DE", "EXXT.DE", "ERNE.DE", "XD9U.DE", "IQQH.DE",
#              "DBX1.DE", "IS3N.DE", "SXR8.DE", "XDUK.DE", "EXS1.DE",
#              "SPYW.DE", "CSEM.DE", "EUNS.DE", "CSPX.DE", "SXRJ.DE")

#
# tickers <- c(
#   "EUNL.DE", "EXXT.DE", "ERNE.DE", "XD9U.DE", "IQQH.DE", "DBX1.DE", "IS3N.DE", "SXR8.DE", "XDUK.DE", "EXS1.DE",
#   "SPYW.DE", "CSEM.DE", "EUNS.DE", "CSPX.DE", "SXRJ.DE", "VUSA.DE", "VUAA.DE", "VEUR.DE", "VGVE.DE", "VGEA.DE",
#   "EUNM.DE", "EUNH.DE", "IBCI.DE", "XDWD.DE", "XMME.DE", "XDJP.DE", "XDEX.DE", "XDEM.DE", "XCS6.DE", "XCS5.DE",
#   "CEMB.DE", "CEMG.DE", "CEUG.DE", "CS51.DE", "XESC.DE", "XMML.DE", "XDWT.DE", "XUEM.DE", "XDUS.DE", "XD9A.DE",
#   "XD7S.DE", "XDWT.DE", "ISPY.DE", "SPPX.DE", "SPXS.DE", "XD5E.DE", "XDW0.DE", "XD9W.DE", "XDWE.DE", "XD9Z.DE",
#   "XD6U.DE", "XD6E.DE", "XD9G.DE", "XD9F.DE", "XD9D.DE", "XD9C.DE", "XDUK.DE", "XDER.DE", "XDBE.DE", "XDDF.DE",
#   "EQQQ.DE", "IQQW.DE", "IQQD.DE", "IQQE.DE", "IQQA.DE", "IQQH.DE", "IQQJ.DE", "IQQK.DE", "IQQT.DE", "IQQN.DE",
#   "SXR1.DE", "SXR2.DE", "SXR3.DE", "SXR4.DE", "SXR5.DE", "SXR6.DE", "SXR7.DE", "SXR8.DE", "SXR9.DE", "SXRV.DE",
#   "SDJP.DE", "SDUS.DE", "SUSM.DE", "SUSL.DE", "SEMA.DE", "SEMG.DE", "SEML.DE", "SEMR.DE", "SEMT.DE", "SEMX.DE",
#   "ZPRV.DE", "ZPRS.DE", "ZPRA.DE", "ZPRB.DE", "ZPRC.DE", "ZPRD.DE", "ZPRE.DE", "ZPRF.DE", "ZPRG.DE", "ZPRH.DE",
#   "XS6R.DE", "XS6S.DE", "XS6T.DE", "XS6U.DE", "XS6V.DE", "XS6W.DE", "XS6X.DE", "XS6Y.DE", "XS6Z.DE", "XS70.DE",
#   "XD6F.DE", "XD6G.DE", "XD6H.DE", "XD6J.DE", "XD6K.DE", "XD6L.DE", "XD6M.DE", "XD6N.DE", "XD6O.DE", "XD6P.DE"
# )

tickers <- c(
  "VUSA.AS",
  "ERNE.AS",
  # Eksisterende ETF'er
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
  "XD6F.DE", "XD6G.DE", "XD6H.DE", "XD6J.DE", "XD6K.DE", "XD6L.DE", "XD6M.DE", "XD6N.DE", "XD6O.DE", "XD6P.DE",

  # Yderligere ETF'er med KID
  "IMEU.DE", # iShares Core MSCI Europe UCITS ETF
  "EXSA.DE", # iShares STOXX Europe 600 UCITS ETF
  "EUN.DE",  # iShares STOXX Europe 50 UCITS ETF
  "ERO.FP",  # SPDR MSCI Europe UCITS ETF
  "VGEU.DE", # Vanguard FTSE Europe ETF
  "DBXG.DE", # Xtrackers MSCI Europe UCITS ETF
  "SXRW.DE", # iShares MSCI World UCITS ETF
  "EUNA.DE", # iShares MSCI Europe ex-UK UCITS ETF
  "XMEU.DE", # Xtrackers Euro Stoxx 50 UCITS ETF
  "VUSA.DE", # Vanguard S&P 500 UCITS ETF

  # Europæiske aktier fra Euronext 100
  "AI.PA",   # Air Liquide
  "AIR.PA",  # Airbus
  "MC.PA",   # LVMH
  "SAN.PA",  # Sanofi
  "OR.PA",   # L'Oréal
  "BNP.PA",  # BNP Paribas
  "ENEL.MI", # Enel
  "ISP.MI",  # Intesa Sanpaolo
  "UCG.MI",  # UniCredit
  "MBG.DE",  # Mercedes-Benz Group
  "BMW.DE",  # BMW
  "SIE.DE",  # Siemens
  "BAS.DE",  # BASF
  "SAP.DE",  # SAP
  "DTE.DE",  # Deutsche Telekom
  "ADS.DE",  # Adidas
  "VOW.DE",  # Volkswagen
  "RWE.DE",  # RWE
  "DBK.DE",  # Deutsche Bank
  "HEIA.AS", # Heineken
  "UNA.AS",  # Unilever
  "PHIA.AS", # Philips
  "ASML.AS", # ASML Holding
  "ADYEN.AS",# Adyen
  "KPN.AS",  # KPN
  "DSM.AS",  # DSM
  "RAND.AS", # Randstad
  "AKZA.AS", # Akzo Nobel
  "TKWY.AS", # Just Eat Takeaway
  "NOVN.SW", # Novartis
  "ROG.SW",  # Roche
  "NESN.SW", # Nestlé
  "UBSG.SW", # UBS Group
  "CSGN.SW", # Credit Suisse
  "ZURN.SW", # Zurich Insurance
  "ABB.SW",  # ABB
  "SGREN.MC",# Siemens Gamesa
  "ITX.MC",  # Inditex
  "TEF.MC",  # Telefónica
  "BBVA.MC", # BBVA
  "SAN.MC",  # Banco Santander
  "REP.MC",  # Repsol
  "ENG.MC",  # Enagás
  "FER.MC",  # Ferrovial
  "IBE.MC",  # Iberdrola
  "AMS.MC",  # Amadeus IT Group
  "MAP.MC",  # Mapfre
  "REE.MC",  # Red Eléctrica
  "TL5.MC"
 )





get_prices <- function(ticker, from = "2017-01-01") {
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
write.csv(returns_df, "data/monthly_log_returns_2017.csv")

cat("Task done. No of assets: ", ncol(returns_df), "\n")


