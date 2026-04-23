causal_predominance <- function(b, r, se_b, se_r,
                                cov_br = 0,
                                alpha  = 0.05,
                                dist   = c("z", "t"),
                                df     = NULL,
                                labels = NULL) {

  ## -------- input checks --------
  dist <- match.arg(dist)
  if (dist == "t" && (is.null(df) || any(df <= 0))) {
    stop("When dist = 't', 'df' must be a positive number (or vector).")
  }
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("'alpha' must be in (0, 1).")
  }

  ## Recycle to a common length
  n <- max(length(b), length(r), length(se_b), length(se_r), length(cov_br))
  b      <- rep_len(b,      n)
  r      <- rep_len(r,      n)
  se_b   <- rep_len(se_b,   n)
  se_r   <- rep_len(se_r,   n)
  cov_br <- rep_len(cov_br, n)
  if (dist == "t") df <- rep_len(df, n)

  if (any(se_b < 0 | se_r < 0, na.rm = TRUE)) {
    stop("Standard errors must be non-negative.")
  }

  ## -------- delta-method SE --------
  estimate <- b - r
  var_diff <- se_b^2 + se_r^2 - 2 * cov_br

  if (any(var_diff < 0, na.rm = TRUE)) {
    warning("Negative variance for one or more pairs (|cov_br| too large ",
            "relative to se_b, se_r). SE set to NA for those rows.")
    var_diff[var_diff < 0] <- NA_real_
  }
  se_diff <- sqrt(var_diff)

  ## -------- test and CI --------
  stat <- estimate / se_diff

  if (dist == "z") {
    p_value  <- 2 * pnorm(-abs(stat))
    crit     <- qnorm(1 - alpha / 2)
  } else {  # t
    p_value  <- 2 * pt(-abs(stat), df = df)
    crit     <- qt(1 - alpha / 2, df = df)
  }

  ci_lower <- estimate - crit * se_diff
  ci_upper <- estimate + crit * se_diff

  ## -------- assemble output --------
  out <- data.frame(
    estimate = estimate,
    se       = se_diff,
    stat     = stat,
    p_value  = p_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    cov_br   = cov_br,
    stringsAsFactors = FALSE
  )

  ## Rename test-statistic column to match the distribution
  names(out)[names(out) == "stat"] <- if (dist == "z") "z" else "t"

  if (!is.null(labels)) {
    if (length(labels) != n) {
      stop("'labels' must have the same length as the input vectors.")
    }
    out <- cbind(label = labels, out, stringsAsFactors = FALSE)
  }

  out
}

# beta_2 - gamma_2
causal_predominance(b = .010, r = .075, se_b = .042, se_r = .043)

# beta_3 - gamma_3
causal_predominance(b = .061, r = .034, se_b = .047, se_r = .041)