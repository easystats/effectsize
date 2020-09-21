#' @keywords internal
.match.rules <- function(rules, choices) {
  if (is.rules(rules)) {
    return(rules)
  }

  if (!rules %in% names(choices) ||
      !is.character(rules) ||
      length(rules)!=1) {
    stop("'rules' must be ",
         paste0("'",names(choices),"'", collapse = ", "),
         " or an object of type 'rules'.",
         call. = FALSE)
  }

  return(choices[[rules]])
}