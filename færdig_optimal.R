library(ompr)
library(ompr.roi)
library(ROI)
library(ROI.plugin.glpk)
library(ROML.portfolio)
library(ROI)
library(Matrix)
library(dplyr)
library(tibble)

# Læs data (277 aktier, 48 månedlige log-returns)
dat <- read.csv(file = "data/monthly_log_returns.csv", sep = ",",
                header = TRUE, row.names = 1)
dat <- as.data.frame(dat)

# 1) Individuel Sharpe (her sættes rf = 0)
rf <- 0
mean_rets <- colMeans(dat)
sd_rets   <- apply(dat, 2, sd)
sharpe_i  <- (mean_rets - rf) / sd_rets

# 2) Sortér aktier efter Sharpe (faldende)
sharpe_i_sorted <- sort(sharpe_i, decreasing = TRUE)

# 3) Vælg top 40 aktier
top40_names <- names(sharpe_i_sorted)[1:40]

# 4) Udsnit af data kun med top 40
dat_sub <- dat[, top40_names]

# Funktion, der sætter Sharpe-problem op
sharpe_objective <- function(r_mat, rf = 0){
  N  <- NCOL(r_mat)
  mu <- colMeans(r_mat)

  # Constraint-matrix
  Amat <- rbind(
    c(mu - rf, 0),        # (mu - rf)*y = 1
    c(rep(0, N), 1),      # kappa > 0
    c(rep(1, N), -1)      # sum(y_i) - kappa = 0
  )
  var.names <- c(paste0("y_sharpe_aux", seq_len(N)), "kappa_sharpe")

  constraint <- ROI::L_constraint(
    L   = Amat,
    dir = c("==", ">", "=="),
    rhs = c(1, 0, 0),
    names = var.names
  )

  # Kvadratisk matrix (cov) med lille ridge
  mat <- matrix(0, nrow = N + 1, ncol = N + 1)
  mat[1:N, 1:N] <- 2 * cov(r_mat)
  mat[N + 1, N + 1] <-  1e-4

  # Vi "maximerer" => Q = -mat
  objective <- ROI::Q_objective(
    Q = -mat,
    L = rep(0, N + 1)
  )

  list(objective = objective, constraint = constraint)
}

# Byg Sharpe-problem på dat_sub
tmp <- sharpe_objective(dat_sub, rf = 0)

lp <- ROI::OP(maximum = TRUE)
objective(lp) <- tmp$objective

# Short-sell constraint (vægte >= 0)
N_sub <- NCOL(dat_sub)
mat_ss <- cbind(diag(N_sub), 1)
shortsell_constraint <- ROI::L_constraint(
  L   = mat_ss,
  dir = rep(">=", N_sub),
  rhs = rep(0, N_sub)
)

constraints(lp) <- rbind(tmp$constraint, shortsell_constraint)

# Løs problemet
sol <- ROI::ROI_solve(lp, solver = "quadprog")

# Ekstraher løsning
sol_sharpe <- solution(sol)

# Beregn endelige porteføljevægte x = y / kappa
kappa_val <- sol_sharpe["kappa_sharpe"]
y_aux     <- sol_sharpe[1:N_sub]
x_opt_raw <- y_aux / kappa_val

# Rund fx til 3 decimaler
x_opt <- round(x_opt_raw, 3)
names(x_opt) <- colnames(dat_sub)

# Se resultatet
x_opt
res <- tibble(Asset = names(x_opt), Weight = x_opt)
res



# stats -------------------------------------------------------------------

# (1) Middelafkast & kovarians for de 40 aktier
mu_monthly  <- colMeans(dat_sub)
Sigma_sub   <- cov(dat_sub)

# (2) Porteføljens månedlige afkast og risiko
P_mu_month <- sum(x_opt * mu_monthly)  # E[r_p]
P_sd_month <- sqrt(t(x_opt) %*% Sigma_sub %*% x_opt)

# (3) Annualisering for månedlige data
P_mu_annual <- 12 * P_mu_month             # ~ årligt logafkast (approks)
P_sd_annual <- sqrt(12) * P_sd_month
P_var_annual <- P_sd_annual^2

# (4) Print i "pæn" stil
cat("\n--- Porteføljestatistik (årligt) ---\n")
cat(sprintf("Forventet afkast:    %.2f %%\n", 100 * P_mu_annual))
cat(sprintf("Standardafvigelse:   %.2f %%\n", 100 * P_sd_annual))
cat(sprintf("Varians:             %.4f\n", P_var_annual))

# backtest ----------------------------------------------------------------


# Fjern navne og sikr numerisk format
x_vec <- as.numeric(x_opt)

# (1) Månedlige log-afkast for porteføljen
port_monthly_returns <- as.numeric(as.matrix(dat_sub) %*% x_vec)

# (2) Kumulative afkast => exp(cumsum(r_t)) for log-afkast
port_cum_value <- exp(cumsum(port_monthly_returns))

# (3) Plot udviklingen
plot(
  port_cum_value, type = "l",
  main = "Backtest: Kumulativ porteføljeudvikling",
  xlab = "Månedsindeks",
  ylab = "Porteføljeværdi (start = 1)"
)

# alternativt plot --------------------------------------------------------
# (1) Månedlige log-afkast for hver aktie i dat_sub => kumulativ værdi
asset_cum <- exp(
  apply(dat_sub, 2, cumsum)
)

# (2) Plot ALLE aktiernes kumulative udvikling i GRÅ
matplot(
  asset_cum,
  type = "l",
  col = "gray",     # <-- alle aktier får grå farve
  lty = 1,           # fuld streg
  lwd = 1,           # normal tykkelse
  main = "Kumulative afkast for 40 aktier + portefølje",
  xlab = "Månedsindeks",
  ylab = "Værdi (start = 1)"
)

# (3) Porteføljen plottes i RØD og med tykkere linje
lines(
  port_cum_value,
  col = "red",
  lwd = 2
)

# (4) Legend (kun for porteføljen hvis du vil holde det minimalistisk)
legend(
  "topleft",
  legend = c("Portefølje"),
  col    = c("red"),
  lty    = 1,
  lwd    = 2,
  bty    = "n"
)
