#' @keywords internal
.match.rules <- function(rules, choices) {
  if (is.rules(rules)) {
    return(rules)
  }

  rule <- pmatch(rules, names(choices))

  if (!is.character(rules) || length(rules) != 1 || is.na(rule)) {
    insight::format_error(
      sprintf(
        "'rules' must be %s or an object of type 'rules'.",
        paste0("'", names(choices), "'", collapse = ", ")
      )
    )
  }

  choices[[rule]]
}
