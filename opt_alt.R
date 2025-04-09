library(ompr)
library(ompr.roi)
library(ROI)
library(ROI.plugin.glpk)
library(ROML.portfolio)
library("ROI")
library(Matrix)

dat <- read.csv(file = "data/monthly_log_returns.csv", sep = ",",
                header = TRUE, row.names = 1)
dat <- as.data.frame(dat)




rf <- 0  # evt. indsæt din risikofri rente her
mean_rets <- colMeans(dat)
sd_rets   <- apply(dat, 2, sd)
sharpe_i  <- (mean_rets - rf) / sd_rets   # vektor med 277 Sharpe ratios

# 2) Sortér aktierne efter Sharpe (fra høj til lav)
sharpe_i_sorted <- sort(sharpe_i, decreasing = TRUE)

# 3) Tag top 40 navne:
top40_names <- names(sharpe_i_sorted)[1:40]

# 4) Opret nyt datasæt med kun de 40 aktier
dat_sub <- dat[, top40_names]





sharpe_objective <- function(r_mat, rf = 0){
  N <- NCOL(r_mat)
  S <- NROW(r_mat)
  mu <- colMeans(r_mat)

  # Ledsager-variable + 'kappa_sharpe'
  Amat <- rbind(c(mu - rf, 0),
                c(rep(0, N), 1),
                c(rep(1, N), -1))
  var.names <- c(paste0("y_sharpe_aux", seq_len(N)), "kappa_sharpe")

  constraint <- ROI::L_constraint(
    L = Amat,
    dir = c("==", ">", "=="),
    rhs = c(1, 0, 0),
    names = var.names
  )

  # Kvadratisk del af objektfunktionen
  mat <- matrix(0, ncol = N + 1, nrow = N + 1)
  mat[1:N, 1:N] <- 2 * cov(r_mat)
  # Lille ridge på diagonalen
  mat[N + 1, N + 1] <-  1e-4

  # Q_objective forventer en (semi)definit Q,
  # men vi sætter fortegn for at 'maximere'
  objective <- ROI::Q_objective(Q = - mat,
                                L = c(rep(0, N), 0))

  list(objective = objective, constraint = constraint)
}

N <- NCOL(dat_sub)
tmp <- sharpe_objective(dat_sub)



lp <- ROI::OP(maximum = TRUE)

objective(lp) <- tmp$objective

mat_ss <- cbind(diag(N), 1)  # til short-sell constraint
shortsell_constraint <- ROI::L_constraint(
  L   = mat_ss,
  dir = rep(">=", N),
  rhs = rep(0, N)
)

constraints(lp) <- rbind(tmp$constraint, shortsell_constraint)

sol <- ROI::ROI_solve(lp, solver = "quadprog")

sol_sharpe <- solution(sol)
x_opt <- round(sol_sharpe[1:40]/sol_sharpe["kappa_sharpe"], 3)
names(x_opt) <-colnames(dat_sub)
x_opt

res <- tibble(x_opt)

mu_monthly <- colMeans(dat_sub)
Sigma_sub <- cov(dat_sub)
sd_port_month <- sqrt( t(x_opt) %*% Sigma_sub %*% x_opt )


P_mu <- sum(x_opt * mu_monthly)
P_sd <- sd_port_month


r_port_annual    <- 12 * P_mu
sd_port_annual   <- sqrt(12) * P_sd
var_port_annual  <- sd_port_annual^2


