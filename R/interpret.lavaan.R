#' Interpretation for lavaan objects
#'
#' @param x An object of class \code{lavaan}.
#' @inheritParams interpret
#'
#' @examples
#' library(effectsize)
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
interpret.lavaan <- function(x, ...) {
  interpret(performance::model_performance(x, ...), ...)
}


#' @export
interpret.performance_lavaan <- function(x, ...){
  table <- data.frame(Name = c(), Value = c(), Interpretation = c(), Threshold = c())

  # GFI
  if("GFI" %in% names(x)){
    table <- rbind(table,
                        data.frame(Name = "GFI",
                                   Value = x$GFI,
                                   Interpretation = interpret_gfi(x$GFI),
                                   Threshold = 0.95))
  }

  # AGFI
  if("AGFI" %in% names(x)){
    table <- rbind(table,
                        data.frame(Name = "AGFI",
                                   Value = x$AGFI,
                                   Interpretation = interpret_agfi(x$AGFI),
                                   Threshold = 0.90))
  }

  # NFI
  if("NFI" %in% names(x)){
    table <- rbind(table,
                        data.frame(Name = "NFI",
                                   Value = x$NFI,
                                   Interpretation = interpret_nfi(x$NFI, rules = "byrne1994"),
                                   Threshold = 0.90))
  }

  # NNFI
  if("NNFI" %in% names(x)){
    table <- rbind(table,
                        data.frame(Name = "NNFI",
                                   Value = x$NNFI,
                                   Interpretation = interpret_nnfi(x$NNFI, rules = "byrne1994"),
                                   Threshold = 0.90))
  }

  # CFI
  if("CFI" %in% names(x)){
    table <- rbind(table,
                        data.frame(Name = "CFI",
                                   Value = x$CFI,
                                   Interpretation = interpret_cfi(x$CFI),
                                   Threshold = 0.90))
  }

  # RMSEA
  if("RMSEA" %in% names(x)){
    table <- rbind(table,
                        data.frame(Name = "RMSEA",
                                   Value = x$RMSEA,
                                   Interpretation = interpret_rmsea(x$RMSEA),
                                   Threshold = 0.05))
  }

  # SRMR
  if("SRMR" %in% names(x)){
    table <- rbind(table,
                        data.frame(Name = "SRMR",
                                   Value = x$SRMR,
                                   Interpretation = interpret_srmr(x$SRMR),
                                   Threshold = 0.08))
  }

  # RFI
  if("RFI" %in% names(x)){
    table <- rbind(table,
                        data.frame(Name = "RFI",
                                   Value = x$RFI,
                                   Interpretation = interpret_rfi(x$RFI),
                                   Threshold = 0.90))
  }

  # IFI
  if("IFI" %in% names(x)){
    table <- rbind(table,
                        data.frame(Name = "IFI",
                                   Value = x$IFI,
                                   Interpretation = interpret_ifi(x$IFI),
                                   Threshold = 0.90))
  }

  # IFI
  if("PNFI" %in% names(x)){
    table <- rbind(table,
                        data.frame(Name = "PNFI",
                                   Value = x$PNFI,
                                   Interpretation = interpret_pnfi(x$PNFI),
                                   Threshold = 0.50))
  }

  table
}