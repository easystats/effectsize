#' Convert H statistic to Rank Epsilon / Eta Squared
#'
#' @param H Kruskal-Wallis rank sum test statistic. See [stats::kruskal.test()]
#' @param N Total sample size
#' @param k Number of groups
#'
#' @export
h_to_epsilon <- function(H, N) {
  H / ((N^2 - 1) / (N + 1))
}

#' @export
#' @rdname h_to_epsilon
h_to_epsilon <- function(H, N, k) {
  (H - k + 1) / (N - k)
}

# TODO use these internally