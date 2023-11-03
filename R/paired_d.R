paired_d <- function(x, y,
                     data = NULL,
                     mu = 0, type = c("rm", "av", "z", "b", "d", "r"),
                     adjust = TRUE,
                     ci = 0.95, alternative = "two.sided",
                     verbose = TRUE, ...) {

  alternative <- effectsize:::.match.alt(alternative)
  type <- match.arg(type)
  data <- .get_data_paired(x, y, data = data, type = type, verbose = verbose, ...)

  if (type %in% c("d", "r")) {
    values <- .replication_d(data, mu = mu, type = type)
  } else {
    values <- .paired_d(data, mu = mu, type = type)
  }

  out <- data.frame(d = values[["d"]])

  if (effectsize:::.test_ci(ci)) {
    # Add cis
    out$CI <- ci
    ci.level <- effectsize:::.adjust_ci(ci, alternative)

    alpha <- 1 - ci.level
    probs <- c(alpha / 2, 1 - alpha / 2)
    qs <- stats::qnorm(probs)

    confint <- out[["d"]] + qs * values[["se"]]
    out$CI_low <- confint[1]
    out$CI_high <- confint[2]

    ci_method <- list(method = "normal")
    out <- effectsize:::.limit_ci(out, alternative, -Inf, Inf)
  } else {
    ci_method <- alternative <- NULL
  }


  if (adjust) {
    df <- values[["df"]]
    J <- exp(lgamma(df / 2) - log(sqrt(df / 2)) - lgamma((df - 1) / 2)) # exact method

    out[, colnames(out) %in% c("d", "CI_low", "CI_high")] <-
      out[, colnames(out) %in% c("d", "CI_low", "CI_high")] * J
  }

  # rename column to type
  colnames(out)[1] <- paste0("d_", type)

  class(out) <- c("effectsize_difference", "effectsize_table", "see_effectsize_table", class(out))
  effectsize:::.someattributes(out) <- effectsize:::.nlist(
    mu, ci, ci_method, alternative,
    approximate = FALSE
  )
  return(out)
}

#' @keywords internal
.get_data_paired <- function(x, y = NULL, data = NULL, type,
                             verbose = TRUE, ...) {
  if (inherits(x, "formula")) {
    formula_error <-
      "Formula must have one of the following forms:
              y ~ condition | id
      Pair(x,y) ~ 1"

    # Validate:
    if (length(x) != 3L) {
      insight::format_error(formula_error)
    } else if (length(x[[3]]) == 3L && x[[3]][[1]] == as.name("|")) {
      # is long
      x[[3L]][[1L]] <- as.name("+")
      mf <- effectsize:::.resolve_formula(x, data, ...)

      if (type %in% c("d", "r")) {
        mf[[2]] <- as.factor(mf[[2]])
        mf[[3]] <- as.factor(mf[[3]])
        colnames(mf) <- c("y", "condition", "id")
        return(stats::na.omit(mf))
      }

      mf <- tapply(mf[[1]], mf[3:2], mean, na.rm = TRUE)
      x <- mf[,1]
      y <- mf[,2]
    } else if (x[[2]][[1]] == as.name("Pair")) {
      # is Pair (wide)
      mf <- effectsize:::.resolve_formula(x, data, ...)
      if (ncol(mf) != 1L) {
        insight::format_error(formula_error)
      }
      x <- mf[[1]]
    } else {
      insight::format_error(formula_error)
    }
  } else {
    # Test if they are they are column names
    x <- effectsize:::.resolve_char(x, data)
    y <- effectsize:::.resolve_char(y, data)
  }

  if (inherits(x, "Pair")) {
    y <- x[, 2]
    x <- x[, 1]
  }

  # x should be a numeric vector or a Pair:
  if (!is.numeric(x) || !is.numeric(y)) {
    insight::format_error("Cannot compute effect size for a non-numeric vector.")
  }

  o <- stats::complete.cases(x, y)
  x <- x[o]
  y <- y[o]

  if (type == "r") {
    insight::format_error("d{r} requires replications.")
  } else if (type == "d") {
    n <- length(x)
    data <- data.frame(
      y = c(x, y),
      condition = factor(rep(1:2, each = n)),
      id = factor(rep(seq(n), times = 2))
    )
    return(data)
  }

  list(x = x, y = y)
}

.paired_d <- function(data, mu, type) {
  x <- data[["x"]]
  y <- data[["y"]]

  m <- mean(x - y)
  n <- length(x)
  df <- n - 1
  r <- cor(x, y)

  if (type == "rm") {
    f <- 2 * (1 - r)

    s <- sd(x - y) / sqrt(f)
    d <- (m - mu) / s

    se <- sqrt(((1 / n) + (d ^ 2) / (2 * n)) * f)
  } else if (type == "av") {
    s <- sqrt((var(x) + var(y)) / 2)
    d <- (m - mu) / s

    se <- sqrt((2 / n) + (d ^ 2) / (4 * n))
  } else if (type == "z") {
    s <- sd(x - y)
    d <- (m - mu) / s

    se <- sqrt((1 / n) + (d ^ 2) / (2 * n))
  } else if (type == "b") {
    s <- sd(x)
    d <- (m - mu) / s

    se <- sqrt((2 * (1 - r) / n) + (d ^ 2) / (2 * n))
  }

  effectsize:::.nlist(d, se, df)
}

.replication_d <- function(data, mu, type) {
  if (type == "r") {
    # for r - need to make sure there are replications!
    cell_ns <- tapply(data[[1]], data[3:2], function(v) length(stats::na.omit(v)))
    all(cell_ns > 1L)
  }

  mod <- stats::aov(y ~ condition + Error(id / condition), data = data,
                    contrasts = list(condition = contr.treatment))

  m <- -unname(coef(mod[["id:condition"]]))
  m_V <- unname(vcov(mod[["id:condition"]])[1])

  pars <- parameters::model_parameters(mod)

  if (type == "d") {
    e <- as.data.frame(pars[pars$Parameter == "Residuals",])
  } else if (type == "r") {
    e <- as.data.frame(pars[pars$Group == "Within",])
  }

  s <- sqrt(sum(e[["Sum_Squares"]]) / sum(e[["df"]]))
  df <- sum(e[["df"]])

  d <- (m - mu) / s
  se <- sqrt((df / (df - 2)) * (m_V / (s^2)) + (d ^ 2) * (8 * df ^2 - df + 2) / (16*(df-2)*((df-1)^2)))

  effectsize:::.nlist(d, se, df)
}

# examples ----------------------------------------

# Paired data
data(sleep)

paired_d(Pair(extra[group == 1], extra[group == 2]) ~ 1, data = sleep, type = "rm")
#> d_rm  |         95% CI
#> ----------------------
#> -0.75 | [-1.17, -0.33]
paired_d(Pair(extra[group == 1], extra[group == 2]) ~ 1, data = sleep, type = "av")
#> d_av  |        95% CI
#> ---------------------
#> -0.76 | [-1.60, 0.07]
paired_d(Pair(extra[group == 1], extra[group == 2]) ~ 1, data = sleep, type = "z")
#> d_z   |         95% CI
#> ----------------------
#> -1.17 | [-1.94, -0.41]
paired_d(Pair(extra[group == 1], extra[group == 2]) ~ 1, data = sleep, type = "b")
#> d_b   |         95% CI
#> ----------------------
#> -0.81 | [-1.31, -0.30]

paired_d(Pair(extra[group == 1], extra[group == 2]) ~ 1, data = sleep, type = "d")
#> d_d   |         95% CI
#> ----------------------
#> -0.80 | [-1.30, -0.30]


dat <- read.table("effectSizePuzzler.txt", header = TRUE)

paired_d(rt ~ cond | id, data = dat, type = "rm")
#> d_rm  |         95% CI
#> ----------------------
#> -0.80 | [-1.06, -0.53]
paired_d(rt ~ cond | id, data = dat, type = "av", adjust = FALSE) # jakewestfall.org/blog: 0.84
#> d_av  |         95% CI
#> ----------------------
#> -0.84 | [-1.41, -0.26]
paired_d(rt ~ cond | id, data = dat, type = "z", adjust = FALSE) # jakewestfall.org/blog: 1.35
#> d_z   |         95% CI
#> ----------------------
#> -1.35 | [-1.90, -0.81]
paired_d(rt ~ cond | id, data = dat, type = "b")
#> d_b   |         95% CI
#> ----------------------
#> -0.86 | [-1.19, -0.53]

paired_d(rt ~ cond | id, data = dat, type = "d") # jakewestfall.org/blog: 0.25
#> d_d   |         95% CI
#> ----------------------
#> -0.25 | [-0.32, -0.18]

paired_d(rt ~ cond | id, data = dat, type = "r") # jakewestfall.org/blog: 0.26
#> d_r   |         95% CI
#> ----------------------
#> -0.26 | [-0.33, -0.18]
