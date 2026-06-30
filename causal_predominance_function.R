## ============================================================================
## Causal predominance test via the delta method (from Mplus output)
## ----------------------------------------------------------------------------
## For a (latent) cross-lagged panel model, the *causal predominance* at a given
## wave is the difference between the two STANDARDIZED cross-lagged effects:
##
##        b = effect  Y -> X      (e.g. SA -> HO)
##        r = effect  X -> Y      (e.g. HO -> SA)
##        causal predominance  =  b - r
##
## Its standard error must account for the COVARIANCE between the two
## standardized paths:
##
##        Var(b - r) = Var(b) + Var(r) - 2 Cov(b, r).
##
## Mplus reports a standardized SE for each path separately but NOT the
## covariance between them, so the difference cannot be tested from the printed
## output alone. This script reconstructs the full covariance matrix among the
## standardized estimates with the delta method,
##
##        Sigma_std = J %*% VCOV %*% t(J),
##
## where VCOV is the parameter covariance (TECH3) and J is the Jacobian of the
## STDYX standardization with respect to the free parameters. From Sigma_std the
## SE of b - r (or any linear contrast of standardized estimates) follows
## directly.
##
## REQUIREMENTS
##   * The Mplus .out MUST be produced with:   OUTPUT: TECH1 TECH3 STDYX;
##     (TECH3 = parameter covariance is required; TECH4 cannot replace it.)
##   * Continuous outcomes, single group.
##   * R packages: MplusAutomation, numDeriv.
##
## Validated against Mplus's own STDYX solution (SEs match to 3 d.p.). MODEL
## CONSTRAINT equality constraints are supported: they make VCOV singular, which
## the delta method (J VCOV J', no inverse of VCOV) handles without trouble.
## ============================================================================

library(MplusAutomation)
library(numDeriv)


## ----------------------------------------------------------------------------
## delta_std_solution(out_file)
##   Reconstruct the STDYX solution AND the full covariance among all
##   standardized estimates from one Mplus .out.
##
##   Returns a list:
##     est_std      named numeric; standardized estimates
##     SE_std       named numeric; delta-method standardized SEs
##     Sigma_std    matrix; full covariance among the standardized estimates
##     lab          character; row/column labels, "HEADER param"
##                  (exactly as Mplus prints them, e.g. "WX2.ON WY1")
##     std_solution data.frame; delta est/se beside Mplus est/se (for checking)
## ----------------------------------------------------------------------------
delta_std_solution <- function(out_file) {

  m <- readModels(out_file)

  ## --- requirements -------------------------------------------------------
  if (is.null(m$tech3$paramCov))
    stop(out_file, ": TECH3 (parameter covariance) missing -- ",
         "set OUTPUT: TECH1 TECH3 STDYX; (TECH4 cannot be used).")
  if (is.null(m$parameters$stdyx.standardized))
    stop(out_file, ": STDYX solution missing -- add STDYX to OUTPUT.")

  ## --- inputs -------------------------------------------------------------
  VCOV <- m$tech3$paramCov
  VCOV[upper.tri(VCOV)] <- t(VCOV)[upper.tri(VCOV)]   # TECH3 lower-tri -> symmetric
  ps   <- m$tech1$parameterSpecification
  spec <- if (!is.null(ps$lambda)) ps else ps[[1]]    # matrices direct, or nested in a group
  obs  <- rownames(spec$lambda); lat <- colnames(spec$lambda)
  p <- length(obs); q <- length(lat)

  ## --- map an Mplus (header, param) to a LISREL matrix CELL ----------------
  locate <- function(header, param) {
    if (grepl("\\.BY$", header)) { f <- sub("\\.BY$", "", header)
      if (param %in% obs) return(list("lambda", param, f)) else return(list("beta", param, f)) }
    if (grepl("\\.ON$", header)) { d <- sub("\\.ON$", "", header); return(list("beta", d, param)) }
    if (grepl("\\.WITH$", header)) { v <- sub("\\.WITH$", "", header)
      if (v %in% lat) return(list("psi", v, param)) else return(list("theta", v, param)) }
    if (header %in% c("Intercepts", "Means")) {
      if (param %in% obs) return(list("nu", 1, param)) else return(list("alpha", 1, param)) }
    if (header %in% c("Variances", "Residual.Variances")) {
      if (param %in% lat) return(list("psi", param, param)) else return(list("theta", param, param)) }
    stop("unhandled header: ", header)
  }
  ## TECH1 number for a cell (symmetric matrices stored in one triangle only)
  spec_num <- function(loc) { mat <- loc[[1]]; i <- loc[[2]]; j <- loc[[3]]
    if (mat %in% c("nu", "alpha")) return(spec[[mat]][1, j])
    v <- spec[[mat]][i, j]
    if ((is.na(v) || v == 0) && mat %in% c("psi", "theta")) v <- spec[[mat]][j, i]
    v }

  ## --- free (se>0) vs FIXED (se==0) parameters ----------------------------
  u <- m$parameters$unstandardized; u <- u[!is.na(u$se), ]
  u_free <- u[u$se != 0, ]; u_fix <- u[u$se == 0, ]

  ## TECH1 number for each free param. With EQUALITY constraints (e.g. item- or
  ## scale-level metric invariance imposed by Mplus equality labels), several
  ## reported params share ONE number -> there can be more free rows than
  ## paramCov columns. We therefore index the free vector x by TECH1 NUMBER
  ## (1..N), and constrained-equal params simply read the same x entry.
  N   <- nrow(VCOV)
  num <- mapply(function(h, pa) spec_num(locate(h, pa)), u_free$paramHeader, u_free$param)
  stopifnot(!any(is.na(num)), all(num >= 1 & num <= N),
            all(sort(unique(num)) == seq_len(N)))     # every paramCov column covered

  ## one free value per unique TECH1 number, in paramCov order (1..N)
  x0 <- numeric(N); x0[num] <- u_free$est             # equality rows carry the same est
  se_by_num <- numeric(N); se_by_num[num] <- u_free$se
  stopifnot(max(abs(round(sqrt(diag(VCOV)), 3) - se_by_num)) < 1e-9)  # SE alignment (3 d.p.)

  ## each free row -> (cell, its TECH1 number);  fixed rows -> injected constants
  free_loc <- lapply(seq_len(nrow(u_free)), function(k)
                 list(loc = locate(u_free$paramHeader[k], u_free$param[k]), num = num[k]))
  fix_loc  <- lapply(seq_len(nrow(u_fix)),  function(k) locate(u_fix$paramHeader[k], u_fix$param[k]))

  ## --- build(): scatter free vector x + injected fixed values into matrices --
  build <- function(x) {
    Lam <- matrix(0, p, q, dimnames = list(obs, lat)); Bet <- matrix(0, q, q, dimnames = list(lat, lat))
    Psi <- matrix(0, q, q, dimnames = list(lat, lat)); The <- matrix(0, p, p, dimnames = list(obs, obs))
    nu  <- setNames(numeric(p), obs); al <- setNames(numeric(q), lat)
    put <- function(loc, val) { mat <- loc[[1]]; i <- loc[[2]]; j <- loc[[3]]
      switch(mat, lambda = Lam[i, j] <<- val, beta = Bet[i, j] <<- val,
        psi = { Psi[i, j] <<- val; Psi[j, i] <<- val }, theta = { The[i, j] <<- val; The[j, i] <<- val },
        nu = nu[j] <<- val, alpha = al[j] <<- val) }
    for (k in seq_along(free_loc)) put(free_loc[[k]]$loc, x[free_loc[[k]]$num])  # FREE (equality -> shared x)
    for (k in seq_along(fix_loc))  put(fix_loc[[k]],  u_fix$est[k])              # FIXED -> injected constant
    list(Lambda = Lam, Beta = Bet, Psi = Psi, Theta = The, nu = nu, alpha = al)
  }

  ## --- STDYX standardization of every reported parameter, as a fn of x -----
  ss <- m$parameters$stdyx.standardized              # target list (report order)
  stdize <- function(x) {
    M <- build(x); IB <- solve(diag(q) - M$Beta)
    Veta <- IB %*% M$Psi %*% t(IB)                    # total latent covariance
    VY   <- M$Lambda %*% Veta %*% t(M$Lambda) + M$Theta
    sdE <- sqrt(diag(Veta)); names(sdE) <- lat
    sdY <- sqrt(diag(VY));   names(sdY) <- obs
    vapply(seq_len(nrow(ss)), function(k) {
      h <- ss$paramHeader[k]; pa <- ss$param[k]
      if (grepl("\\.BY$", h)) { f <- sub("\\.BY$", "", h)
        if (pa %in% obs) return(M$Lambda[pa, f] * sdE[f] / sdY[pa])
        else             return(M$Beta[pa, f]  * sdE[f] / sdE[pa]) }
      if (grepl("\\.ON$", h)) { d <- sub("\\.ON$", "", h)
        return(M$Beta[d, pa] * sdE[pa] / sdE[d]) }
      if (grepl("\\.WITH$", h)) { v <- sub("\\.WITH$", "", h)
        if (v %in% lat) return(M$Psi[v, pa]   / sqrt(M$Psi[v, v]   * M$Psi[pa, pa]))
        else            return(M$Theta[v, pa] / sqrt(M$Theta[v, v] * M$Theta[pa, pa])) }
      if (h %in% c("Intercepts", "Means")) {
        if (pa %in% obs) return(M$nu[pa] / sdY[pa]) else return(M$alpha[pa] / sdE[pa]) }
      if (h %in% c("Variances", "Residual.Variances")) {
        if (pa %in% lat) return(M$Psi[pa, pa] / Veta[pa, pa]) else return(M$Theta[pa, pa] / VY[pa, pa]) }
      NA_real_
    }, numeric(1))
  }

  ## --- delta method:  Sigma_std = J VCOV J' -------------------------------
  est_std   <- stdize(x0)
  J         <- jacobian(stdize, x0)
  Sigma_std <- J %*% VCOV %*% t(J)
  SE_std    <- sqrt(diag(Sigma_std))

  lab <- paste(ss$paramHeader, ss$param)             # e.g. "WX2.ON WY1"
  names(est_std) <- names(SE_std) <- lab
  dimnames(Sigma_std) <- list(lab, lab)

  std_solution <- data.frame(header = ss$paramHeader, param = ss$param,
                             est.std = round(est_std, 3), se = round(SE_std, 3),
                             est_mp = ss$est, se_mp = ss$se, row.names = NULL)

  list(est_std = est_std, SE_std = SE_std, Sigma_std = Sigma_std,
       lab = lab, std_solution = std_solution)
}


## ----------------------------------------------------------------------------
## causal_predominance(object, waves, digits = 3)
##   Test the standardized difference  b - r  at one or more waves.
##
##   object : either a path to an Mplus .out, OR a list returned by
##            delta_std_solution() (reuse it to avoid re-reading the file).
##   waves  : named list. Each element is a length-2 character vector
##            c(b = "<b label>", r = "<r label>") giving the two cross-lagged
##            paths to compare, in "HEADER param" form, EXACTLY as Mplus prints
##            them in the STDYX section. Example for X = HO, Y = SA:
##              waves <- list(
##                "wave 1->2" = c(b = "WX2.ON WY1", r = "WY2.ON WX1"),
##                "wave 2->3" = c(b = "WX3.ON WY2", r = "WY3.ON WX2"))
##   digits : rounding for the printed estimates (covariance always 6 d.p.).
##
##   Returns a data.frame with one row per wave:
##     wave, b, r, cov_b_r, diff (= b - r), se, z, p (two-sided).
##     diff > 0  => Y->X predominates ;  diff < 0  => X->Y predominates.
## ----------------------------------------------------------------------------
causal_predominance <- function(object, waves, digits = 3) {

  fit <- if (is.character(object)) delta_std_solution(object) else object
  stopifnot(all(c("est_std", "Sigma_std", "lab") %in% names(fit)))

  est_std <- fit$est_std; Sigma_std <- fit$Sigma_std; lab <- fit$lab

  do.call(rbind, lapply(names(waves), function(w) {
    b_lab <- waves[[w]]["b"]; r_lab <- waves[[w]]["r"]
    if (!(b_lab %in% lab)) stop("label not found in STDYX solution: ", b_lab)
    if (!(r_lab %in% lab)) stop("label not found in STDYX solution: ", r_lab)

    b      <- est_std[b_lab]                 # standardized Y -> X
    r      <- est_std[r_lab]                 # standardized X -> Y
    var_b  <- Sigma_std[b_lab, b_lab]
    var_r  <- Sigma_std[r_lab, r_lab]
    cov_br <- Sigma_std[b_lab, r_lab]

    diff <- b - r
    se   <- sqrt(var_b + var_r - 2 * cov_br) # Var(b - r) = Var(b)+Var(r)-2Cov(b,r)
    z    <- diff / se

    data.frame(wave = w,
               b = round(b, digits), r = round(r, digits),
               cov_b_r = round(cov_br, 6),
               diff_b_minus_r = round(diff, digits),
               se = round(se, digits), z = round(z, digits),
               p = round(2 * pnorm(-abs(z)), 4), row.names = NULL)
  }))
}


## ----------------------------------------------------------------------------
## std_contrast(object, labels, weights)
##   (bonus) SE / z / p for ANY linear contrast  sum(weights * standardized).
##   e.g. b - r is std_contrast(fit, c("WX2.ON WY1", "WY2.ON WX1"), c(1, -1)).
## ----------------------------------------------------------------------------
std_contrast <- function(object, labels, weights) {
  fit <- if (is.character(object)) delta_std_solution(object) else object
  stopifnot(all(labels %in% fit$lab), length(labels) == length(weights))
  cvec <- setNames(numeric(length(fit$lab)), fit$lab); cvec[labels] <- weights
  est <- sum(cvec * fit$est_std)
  v   <- as.numeric(t(cvec) %*% fit$Sigma_std %*% cvec)
  z   <- est / sqrt(v)
  data.frame(estimate = est, se = sqrt(v), z = z, p = 2 * pnorm(-abs(z)), row.names = NULL)
}


## ============================================================================
## EXAMPLE (runnable)
## ============================================================================
# setwd("C:/Users/wqemi/OneDrive - Florida State University/MyFSU_OneDrive/Documents/FSU/dissertation/applied data for illustration")

# ## --- Multiple-indicator model (within factors WX*, WY*); X = HO, Y = SA ------
# fit_F <- delta_std_solution("lclpm-f_uli_ho_sa_metric.out")
# print(fit_F$std_solution)            # optional: delta-method SEs vs Mplus STDYX

# ## `waves`: name each wave -> the two cross-lagged paths to compare,
# ## b = effect Y -> X (SA -> HO),  r = effect X -> Y (HO -> SA).
# waves_F <- list(
#   "wave 1->2" = c(b = "WX2.ON WY1", r = "WY2.ON WX1"),
#   "wave 2->3" = c(b = "WX3.ON WY2", r = "WY3.ON WX2"))
# causal_predominance(fit_F, waves_F)

# ## --- Composite single-indicator model (within factors WFX*, WFY*) -----------
# waves_CI <- list(
#   "wave 1->2" = c(b = "WFX2.ON WFY1", r = "WFY2.ON WFX1"),
#   "wave 2->3" = c(b = "WFX3.ON WFY2", r = "WFY3.ON WFX2"))
# causal_predominance("lclpm-ci_ho_sa.out", waves_CI)
