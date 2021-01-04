#' - Tomczak, M., & Tomczak, E. (2014). The need to report effect size estimates revisited. An overview of some recommended measures of effect size.
rank_epsilon_squared <- function(x, g, data = NULL, ci = 0.95, iterations = 200) {

  data <- .rank_anova_xg(x, g, data)
  data <- stats::na.omit(data)
  x <- data$x
  g <- data$g

  model <- stats::kruskal.test(x, g)

  H <- model$statistic
  n <- length(g)

  E <- H / ((n^2 - 1)/(n + 1))

  out <- data.frame(rank_epsilon_squared = E)

  if (is.numeric(ci)) {
    warning("Nope. Not yet.")
    out$CI <- ci
    out$CI_low <- 0
    out$CI_high <- 1
  }

  class(out) <- c("effectsize_table", class(out))
  return(out)
}

# rank_eta_squared <- function(x, g, data = NULL, ci = 0.95, iterations = 200) {
#
#   data <- .rank_anova_xg(x, g, data)
#   data <- stats::na.omit(data)
#   x <- data$x
#   g <- data$g
#
#   model <- stats::kruskal.test(x, g)
#
#   H <- unname(model$statistic)
#   k <- length(unique(g)) # model$parameter + 1
#   n <- length(g)
#
#   E <- (H - k + 1) / (n - k)
#
#   out <- data.frame(rank_eta_squared = E)
#
#   if (is.numeric(ci)) {
#     warning("Nope. Not yet.")
#     out$CI <- ci
#     out$CI_low <- 0
#     out$CI_high <- 1
#   }
#
#   class(out) <- c("effectsize_table", class(out))
#   return(out)
# }

.rank_anova_xg <- function(x, g, data) {
  if (inherits(frm <- x, "formula")) {
    if (length(frm) != 3)
      stop("Formula must have the 'outcome ~ group'.", call. = FALSE)

    mf <- stats::model.frame(stats::lm(formula = frm, data = data))

    x <- mf[[1]]
    if (ncol(mf) == 2) {
      g <- factor(mf[[2]])
    } else {
      stop("Formula must have the 'outcome ~ group'.", call. = FALSE)
    }
  } else if (length(x) != length(g)) {
    stop("x and g must be of the same length.", call. = FALSE)
  }

  data.frame(x, g)
}

kendalls_w <- function(y, groups, blocks, data = NULL, ci = 0.95, iterations = 200) {

  data <- .kendalls_w_data(y, groups, blocks, data)
  data <- na.omit(data)
  y <- data$y
  groups <- data$groups
  blocks <- data$blocks

  model <- stats::friedman.test(y, groups, blocks)

  Chi <- unname(model$statistic)
  N <- length(unique(groups))
  k <- length(unique(blocks))

  W <- Chi / (N * (k - 1))

  out <- data.frame(kendalls_w = W)

  if (is.numeric(ci)) {
    warning("Nope. Not yet.")
    out$CI <- ci
    out$CI_low <- 0
    out$CI_high <- 1
  }

  class(out) <- c("effectsize_table", class(out))
  return(out)
}


.kendalls_w_data <- function(y, groups, blocks, data = NULL) {
  if (inherits(frm <- y, "formula")) {
    if ((length(frm) != 3L) ||
        (length(frm[[3L]]) != 3L) ||
        (frm[[3L]][[1L]] != as.name("|")) ||
        (length(frm[[3L]][[2L]]) != 1L) ||
        (length(frm[[3L]][[3L]]) != 1L))
      stop("Formula must have the 'values ~ groups | blocks'.", call. = FALSE)

    frm[[3L]][[1L]] <- as.name("+")

    mf <- stats::model.frame(stats::lm(formula = frm, data = data))

    y <- mf[[1]]
    groups <- mf[[2]]
    blocks <- mf[[3]]
  } else if (length(y) != length(groups) || length(y) != length(blocks)) {
    stop("y, groups and blocks must be of the same length.", call. = FALSE)
  }

  data.frame(y, groups, blocks)
}



# test --------------------------------------------------------------------

x <- c(2.9, 3.0, 2.5, 2.6, 3.2) # normal subjects
y <- c(3.8, 2.7, 4.0, 2.4)      # with obstructive airway disease
z <- c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis

## Equivalently,
x <- c(x, y, z)
g <- factor(rep(1:3, c(5, 4, 5)),
            labels = c("Normal subjects",
                       "Subjects with obstructive airway disease",
                       "Subjects with asbestosis"))

rank_epsilon_squared(x, g)
rank_epsilon_squared(x ~ g, data = data.frame(x, g))
rcompanion::epsilonSquared(x, g)

rank_eta_squared(x, g) #???



wb <- aggregate(warpbreaks$breaks,
                by = list(w = warpbreaks$wool,
                          t = warpbreaks$tension),
                FUN = mean)

kendalls_w(x ~ w | t, data = wb)
kendalls_w(wb$x, wb$w, wb$t)
