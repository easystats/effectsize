
#' @keywords internal
.get_ncp_F <- function(f, df, df_error, conf.level = 0.9) {
  alpha <- 1 - conf.level
  probs <- c(alpha / 2, 1 - alpha / 2)

  if (isTRUE(all.equal(f, 0))) {
    return(c(0, Inf)) # unestimatable
  }

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
  return(f_ncp)
}

#' @keywords internal
.get_ncp_t <- function(t, df_error, conf.level = 0.95) {
  alpha <- 1 - conf.level
  probs <- c(alpha / 2, 1 - alpha / 2)

  if (isTRUE(all.equal(t, 0))) {
    t_ncp <- qt(probs, df_error)
    return(t_ncp)
  }

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
  if (isTRUE(all.equal(t_ncp[1], 0))) {
    t_ncp[1] <- qt(probs[1], df_error)
  }

  if (isTRUE(all.equal(t_ncp[2], 0))) {
    t_ncp[2] <- qt(probs[2], df_error)
  }
  return(t_ncp)
}
