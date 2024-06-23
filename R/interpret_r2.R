#' Interpret Coefficient of Determination (\eqn{R^2})
#'
#' @param r2 Value or vector of \eqn{R^2} values.
#' @param rules Can be `"cohen1988"` (default), `"falk1992"`, `"chin1998"`,
#'   `"hair2011"`, or custom set of [rules()]].
#'
#' @section Rules:
#'
#' ## For Linear Regression
#'
#' - Cohen (1988) (`"cohen1988"`; default)
#'   - **R2 < 0.02** - Very weak
#'   - **0.02 <= R2 < 0.13** - Weak
#'   - **0.13 <= R2 < 0.26** - Moderate
#'   - **R2 >= 0.26** - Substantial
#' - Falk & Miller (1992) (`"falk1992"`)
#'   - **R2 < 0.1** - Negligible
#'   - **R2 >= 0.1** - Adequate
#'
#' ## For PLS / SEM R-Squared of *latent* variables
#'
#' - Chin, W. W. (1998) (`"chin1998"`)
#'   - **R2 < 0.19** - Very weak
#'   - **0.19 <= R2 < 0.33** - Weak
#'   - **0.33 <= R2 < 0.67** - Moderate
#'   - **R2 >= 0.67** - Substantial
#' - Hair et al. (2011) (`"hair2011"`)
#'   - **R2 < 0.25** - Very weak
#'   - **0.25 <= R2 < 0.50** - Weak
#'   - **0.50 <= R2 < 0.75** - Moderate
#'   - **R2 >= 0.75** - Substantial
#'
#' @examples
#' interpret_r2(.02)
#' interpret_r2(c(.5, .02))
#' @references
#' - Cohen, J. (1988). Statistical power analysis for the behavioral sciences
#' (2nd Ed.). New York: Routledge.
#'
#' - Falk, R. F., & Miller, N. B. (1992). A primer for soft modeling. University
#' of Akron Press.
#'
#' - Chin, W. W. (1998). The partial least squares approach to structural
#' equation modeling. Modern methods for business research, 295(2), 295-336.
#'
#' - Hair, J. F., Ringle, C. M., & Sarstedt, M. (2011). PLS-SEM: Indeed a silver
#' bullet. Journal of Marketing theory and Practice, 19(2), 139-152.
#'
#' @keywords interpreters
#' @export
interpret_r2 <- function(r2, rules = "cohen1988") {
  rules <- .match.rules(
    rules,
    list(
      cohen1988 = rules(c(0.02, 0.13, 0.26), c("very weak", "weak", "moderate", "substantial"),
        name = "cohen1988", right = FALSE
      ),
      falk1992 = rules(0.10, c("negligible", "adequate"),
        name = "falk1992", right = FALSE
      ),
      chin1998 = rules(c(0.19, 0.33, 0.67), c("very weak", "weak", "moderate", "substantial"),
        name = "chin1998", right = FALSE
      ),
      hair2011 = rules(c(0.25, 0.50, 0.75), c("very weak", "weak", "moderate", "substantial"),
        name = "hair2011", right = FALSE
      )
    )
  )

  interpret(r2, rules)
}
