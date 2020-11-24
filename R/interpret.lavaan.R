#' @examples
#' library(report)
#'
#' # Structural Equation Models (SEM)
#' if (require("lavaan")) {
#'   structure <- " ind60 =~ x1 + x2 + x3
#'                  dem60 =~ y1 + y2 + y3
#'                  dem60 ~ ind60 "
#'   model <- lavaan::sem(structure, data = PoliticalDemocracy)
#'   interpret(model)
#' }
#' @export
interpret.lavaan <- function(x, ...){
  perf <- performance::model_performance(x, ...)
}
