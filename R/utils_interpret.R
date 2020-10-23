#' @keywords internal
.match.rules <- function(rules, choices) {
  if (is.rules(rules)) {
    return(rules)
  }

  rule <- pmatch(rules, names(choices))

  if (!is.character(rules) || length(rules) != 1 || is.na(rule)) {
    stop("'rules' must be ",
         paste0("'",names(choices),"'", collapse = ", "),
         " or an object of type 'rules'.",
         call. = FALSE)
  }

  return(choices[[rule]])
}