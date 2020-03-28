
#' @keywords internal
#' @importFrom stats pf
.get_ncp_F <- function(f, df, df_error, conf.level = 0.9) {
  alpha <- 1 - conf.level
  probs <- c(alpha / 2, 1 - alpha / 2)

  lambda <- f * df
  ncp <- suppressWarnings(optim(
    par = 1.1 * rep(lambda, 2),
    fn = function(x) {
      p <- pf(q = f, df, df_error, ncp = x)

      abs(max(p) - probs[2]) +
        abs(min(p) - probs[1])
    },
    control = list(abstol = 1e-09)
  ))
  f_ncp <- sort(ncp$par) / df

  if (f <= qf(probs[2], df, df_error)) {
    f_ncp[1] <- 0
  }

  return(f_ncp)
}

#' @keywords internal
#' @importFrom stats pt
.get_ncp_t <- function(t, df_error, conf.level = 0.95) {
  alpha <- 1 - conf.level
  probs <- c(alpha / 2, 1 - alpha / 2)

  ncp <- suppressWarnings(optim(
    par = 1.1 * rep(t, 2),
    fn = function(x) {
      p <- pt(q = t, df = df_error, ncp = x)

      abs(max(p) - probs[2]) +
        abs(min(p) - probs[1])
    },
    control = list(abstol = 1e-09)
  ))
  t_ncp <- unname(sort(ncp$par))

  return(t_ncp)
}

#' @keywords internals
#' @importFrom stats pchisq
.get_ncp_chi <- function(chi, df, conf.level = 0.95) {
  alpha <- 1 - conf.level
  probs <- c(alpha / 2, 1 - alpha / 2)

  ncp <- suppressWarnings(optim(
    par = 1.1 * rep(chi, 2),
    fn = function(x) {
      p <- pchisq(q = chi, df, ncp = x)

      abs(max(p) - probs[2]) +
        abs(min(p) - probs[1])
    },
    control = list(abstol = 1e-09)
  ))
  chi_ncp <- sort(ncp$par)

  if (chi <= qchisq(probs[2], df)) {
    chi_ncp[1] <- 0
  }

  return(chi_ncp)
}
