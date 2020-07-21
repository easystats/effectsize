#' Interpretation of indices of fit
#'
#' Interpretation of indices of fit found in confirmatory analysis or structural equation modelling, such as RMSEA, CFI, NFI, IFI, etc.
#'
#' @param x vector of values.
#' @param rules Can be "default" or custom set of rules.
#'
#' @inherit performance::model_performance.lavaan details
#' @inherit performance::model_performance.lavaan references
#'
#' @details \subsection{Indices of fit}{
#' \itemize{
#'    \item **Chisq**: The model Chi-squared assesses overall fit and the discrepancy between the sample and fitted covariance matrices. Its p-value should be > .05 (i.e., the hypothesis of a perfect fit cannot be rejected). However, it is quite sensitive to sample size.
#'    \item **GFI/AGFI**: The (Adjusted) Goodness of Fit is the proportion of variance accounted for by the estimated population covariance. Analogous to R2. The GFI and the AGFI should be > .95 and > .90, respectively.
#'    \item **NFI/NNFI/TLI**: The (Non) Normed Fit Index. An NFI of 0.95, indicates the model of interest improves the fit by 95\% relative to the null model. The NNFI (also called the Tucker Lewis index; TLI) is preferable for smaller samples. They should be > .90 (Byrne, 1994) or > .95 (Schumacker & Lomax, 2004).
#'    \item **CFI**: The Comparative Fit Index is a revised form of NFI. Not very sensitive to sample size (Fan, Thompson, & Wang, 1999). Compares the fit of a target model to the fit of an independent, or null, model. It should be > .90.
#'    \item **RMSEA**: The Root Mean Square Error of Approximation is a parsimony-adjusted index. Values closer to 0 represent a good fit. It should be < .08 or < .05. The p-value printed with it tests the hypothesis that RMSEA is less than or equal to .05 (a cutoff sometimes used for good fit), and thus should be not significant.
#'    \item **RMR/SRMR**: the (Standardized) Root Mean Square Residual represents the square-root of the difference between the residuals of the sample covariance matrix and the hypothesized model. As the RMR can be sometimes hard to interpret, better to use SRMR. Should be < .08.
#'    \item **RFI**: the Relative Fit Index, also known as RHO1, is not guaranteed to vary from 0 to 1. However, RFI close to 1 indicates a good fit.
#'    \item **IFI**: the Incremental Fit Index (IFI) adjusts the Normed Fit Index (NFI) for sample size and degrees of freedom (Bollen's, 1989). Over 0.90 is a good fit, but the index can exceed 1.
#'    \item **PNFI**: the Parsimony-Adjusted Measures Index. There is no commonly agreed-upon cutoff value for an acceptable model for this index. Should be > 0.50.
#' }
#' See the documentation for `lavaan::fitmeasures`.
#' }
#'
#'
#' \subsection{What to report}{
#' For structural equation models (SEM), Kline (2015) suggests that at a minimum the following indices should be reported: The model **chi-square**, the **RMSEA**, the **CFI** and the **SRMR**.
#' }
#'
#'
#'
#' @examples
#' interpret_gfi(c(.5, .99))
#' interpret_agfi(c(.5, .99))
#' interpret_nfi(c(.5, .99))
#' interpret_nnfi(c(.5, .99))
#' interpret_cfi(c(.5, .99))
#' interpret_rmsea(c(.5, .99))
#' interpret_srmr(c(.5, .99))
#' interpret_rfi(c(.5, .99))
#' interpret_ifi(c(.5, .99))
#' interpret_pnfi(c(.5, .99))
#' @references \itemize{
#'   \item Awang, Z. (2012). A handbook on SEM. Structural equation modeling.
#'   \item Byrne, B. M. (1994). Structural equation modeling with EQS and EQS/Windows. Thousand Oaks, CA: Sage Publications.
#'   \item Tucker, L. R., \& Lewis, C. (1973). The reliability coefficient for maximum likelihood factor analysis. Psychometrika, 38, 1-10.
#'   \item Schumacker, R. E., \& Lomax, R. G. (2004). A beginner's guide to structural equation modeling, Second edition. Mahwah, NJ: Lawrence Erlbaum Associates.
#'   \item Fan, X., B. Thompson, \& L. Wang (1999). Effects of sample size, estimation method, and model specification on structural equation modeling fit indexes. Structural Equation Modeling, 6, 56-83.
#'   \item Kline, R. B. (2015). Principles and practice of structural equation modeling. Guilford publications.
#' }
#' @export
interpret_gfi <- function(x, rules = "default") {
  if (is.rules(rules)) {
    return(interpret(x, rules))
  } else {
    if (rules == "default") {
      return(interpret(x, rules(c(0.95), c("poor", "satisfactory"))))
    } else {
      stop("rules must be 'default' or an object of type rules.")
    }
  }
}


#' @rdname interpret_gfi
#' @export
interpret_agfi <- function(x, rules = "default") {
  if (is.rules(rules)) {
    return(interpret(x, rules))
  } else {
    if (rules == "default") {
      return(interpret(x, rules(c(0.90), c("poor", "satisfactory"))))
    } else {
      stop("rules must be 'default' or an object of type rules.")
    }
  }
}


#' @rdname interpret_gfi
#' @export
interpret_nfi <- function(x, rules = "byrne1994") {
  if (is.rules(rules)) {
    return(interpret(x, rules))
  } else {
    if (rules == "byrne1994") {
      return(interpret(x, rules(c(0.90), c("poor", "satisfactory"))))
    } else if (rules == "schumacker2004") {
      return(interpret(x, rules(c(0.95), c("poor", "satisfactory"))))
    } else {
      stop("rules must be 'default' or an object of type rules.")
    }
  }
}

#' @rdname interpret_gfi
#' @export
interpret_nnfi <- interpret_nfi


#' @rdname interpret_gfi
#' @export
interpret_cfi <- function(x, rules = "default") {
  if (is.rules(rules)) {
    return(interpret(x, rules))
  } else {
    if (rules == "default") {
      return(interpret(x, rules(c(0.90), c("poor", "satisfactory"))))
    } else {
      stop("rules must be 'default' or an object of type rules.")
    }
  }
}




#' @rdname interpret_gfi
#' @export
interpret_rmsea <- function(x, rules = "default") {
  if (is.rules(rules)) {
    return(interpret(x, rules))
  } else {
    if (rules == "default") {
      return(interpret(x, rules(c(0.05), c("satisfactory", "poor"))))
    } else if (rules == "awang2012") {
      return(interpret(x, rules(c(0.05, 0.08), c("good", "satisfactory", "poor"))))
    } else {
      stop("rules must be 'default' or an object of type rules.")
    }
  }
}


#' @rdname interpret_gfi
#' @export
interpret_srmr <- function(x, rules = "default") {
  if (is.rules(rules)) {
    return(interpret(x, rules))
  } else {
    if (rules == "default") {
      return(interpret(x, rules(c(0.08), c("satisfactory", "poor"))))
    } else {
      stop("rules must be 'default' or an object of type rules.")
    }
  }
}

#' @rdname interpret_gfi
#' @export
interpret_rfi <- function(x, rules = "default") {
  if (is.rules(rules)) {
    return(interpret(x, rules))
  } else {
    if (rules == "default") {
      return(interpret(x, rules(c(0.90), c("poor", "satisfactory"))))
    } else {
      stop("rules must be 'default' or an object of type rules.")
    }
  }
}

#' @rdname interpret_gfi
#' @export
interpret_ifi <- function(x, rules = "default") {
  if (is.rules(rules)) {
    return(interpret(x, rules))
  } else {
    if (rules == "default") {
      return(interpret(x, rules(c(0.90), c("poor", "satisfactory"))))
    } else {
      stop("rules must be 'default' or an object of type rules.")
    }
  }
}

#' @rdname interpret_gfi
#' @export
interpret_pnfi <- function(x, rules = "default") {
  if (is.rules(rules)) {
    return(interpret(x, rules))
  } else {
    if (rules == "default") {
      return(interpret(x, rules(c(0.50), c("poor", "satisfactory"))))
    } else {
      stop("rules must be 'default' or an object of type rules.")
    }
  }
}
