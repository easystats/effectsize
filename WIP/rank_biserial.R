# https://doi.org/10.2466/11.IT.3.1

# stats::kruskal.test
# stats::friedman.test

rank_biserial <- function(x, y = NULL, data = NULL, mu = 0,
                          ci = 0.95, nboot = 200,
                          paired = FALSE) {
  out <- effectsize:::.deal_with_cohens_d_arguments(x, y, data)

  x <- out$x
  y <- out$y

  if (is.null(y)) {
    y <- rep(0, length.out = length(x))
    paired <- TRUE
  }

  if (paired) {
    d <- (x - y) - mu

    r_sign <- effectsize::ranktransform(d, sign = TRUE)
    r_sign <- na.omit(r_sign)

    r_pos <- sum(r_sign[r_sign > 0])
    r_neg <- -sum(r_sign[r_sign < 0])
    T_ <- min(r_pos, r_neg)
    n <- length(r_sign)

    r_rbs <- 4 * abs(T_ - (r_pos + r_neg) / 2) / (n * (n + 1))
    if (r_pos >= r_neg) r_rbs <- -r_rbs
  } else {
    stop("Not yet supported")
  }


  out <- data.frame(r_rank_biserial = r_rbs)

  if (is.numeric(ci)) {
    if (!requireNamespace("boot")) {
      warning("For CIs, the 'boot' package must be installed.")
    } else {
      # Add cis
      out$CI <- ci

      # TODO this needs to be different for paired / one sample data
      # TODO (both data and function call!)
      if (paired) {
        # TODO
      } else {
        temp_dat <- data.frame(x = c(x,y),
                               g = c(rep("a", length(x)),
                                     rep("b", length(y))))

        B <- boot::boot(
          data = temp_dat, R = nboot,
          statistic = function(.data, .i) {
            rank_biserial("x", "g", data = .data[.i,],
                          mu = mu, paired = paired,
                          ci = NULL)[[1]]
          }
        )
      }

      BCI <- boot::boot.ci(B, conf = ci, type = "bca")

      out$CI_low <- BCI$bca[4]
      out$CI_high <- BCI$bca[5]
    }
  }

  class(out) <- c("effectsize_table", class(out))
  return(out)
}

A = c(11,12,13,14,15,16,17,18,19,20)
B = c(12,14,16,18,20,22,12,10,19,20)
Y = c(A, B)
Group = factor(c(rep("A", length(A)),
                 rep("B", length(B))))

rank_biserial(Y, mu = 15)
rank_biserial(Y, Group, paired = TRUE)
