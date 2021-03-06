% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cohens_d.R
\name{cohens_d}
\alias{cohens_d}
\alias{hedges_g}
\alias{glass_delta}
\title{Effect size for differences}
\usage{
cohens_d(
  x,
  y = NULL,
  data = NULL,
  pooled_sd = TRUE,
  mu = 0,
  paired = FALSE,
  ci = 0.95,
  verbose = TRUE,
  ...
)

hedges_g(
  x,
  y = NULL,
  data = NULL,
  pooled_sd = TRUE,
  mu = 0,
  paired = FALSE,
  ci = 0.95,
  verbose = TRUE,
  ...,
  correction
)

glass_delta(
  x,
  y = NULL,
  data = NULL,
  mu = 0,
  ci = 0.95,
  verbose = TRUE,
  ...,
  iterations
)
}
\arguments{
\item{x}{A formula, a numeric vector, or a character name of one in \code{data}.}

\item{y}{A numeric vector, a grouping (character / factor) vector, a or a
character  name of one in \code{data}. Ignored if \code{x} is a formula.}

\item{data}{An optional data frame containing the variables.}

\item{pooled_sd}{If \code{TRUE} (default), a \code{\link[=sd_pooled]{sd_pooled()}} is used (assuming equal
variance). Else the mean SD from both groups is used instead.}

\item{mu}{a number indicating the true value of the mean (or
    difference in means if you are performing a two sample test).}

\item{paired}{If \code{TRUE}, the values of \code{x} and \code{y} are considered as paired.
This produces an effect size that is equivalent to the one-sample effect
size on \code{x - y}.}

\item{ci}{Confidence Interval (CI) level}

\item{verbose}{Toggle warnings and messages on or off.}

\item{...}{Arguments passed to or from other methods.}

\item{iterations, correction}{deprecated.}
}
\value{
A data frame with the effect size ( \code{Cohens_d}, \code{Hedges_g},
\code{Glass_delta}) and their CIs (\code{CI_low} and \code{CI_high}).
}
\description{
Compute effect size indices for standardized differences: Cohen's \emph{d},
Hedges' \emph{g} and Glass’s \emph{delta} (\eqn{\Delta}). (This function returns the
\strong{population} estimate.)
\cr\cr
Both Cohen's \emph{d} and Hedges' \emph{g} are the estimated the standardized
difference between the means of two populations. Hedges' \emph{g} provides a bias
correction (using the exact method) to Cohen's \emph{d} for small sample sizes.
For sample sizes > 20, the results for both statistics are roughly
equivalent. Glass’s \emph{delta} is appropriate when the standard deviations are
significantly different between the populations, as it uses only the \emph{second}
group's standard deviation.
}
\details{
Set \code{pooled_sd = FALSE} for effect sizes that are to accompany a Welch's
\emph{t}-test (Delacre et al, 2021).
}
\note{
The indices here give the population estimated standardized difference.
Some statistical packages give the sample estimate instead (without
applying Bessel's correction).
}
\section{Confidence Intervals}{

Unless stated otherwise, confidence intervals are estimated using the
Noncentrality parameter method; These methods searches for a the best
non-central parameters (\code{ncp}s) of the noncentral t-, F- or Chi-squared
distribution for the desired tail-probabilities, and then convert these
\code{ncp}s to the corresponding effect sizes. (See full \link{effectsize-CIs} for
more.)
}

\section{CI Contains Zero}{

Keep in mind that \code{ncp} confidence intervals are inverted significance tests,
and only inform us about which values are not significantly different than
our sample estimate. (They do \emph{not} inform us about which values are
plausible, likely or compatible with our data.) Thus, when CIs contain the
value 0, this should \emph{not} be taken to mean that a null effect size is
supported by the data; Instead this merely reflects a non-significant test
statistic - i.e. the \emph{p}-value is greater than alpha (Morey et al., 2016).
\cr\cr
For positive only effect sizes (Eta squared, Cramer's V, etc.; Effect sizes
associated with Chi-squared and F distributions), this applies also to cases
where the lower bound of the CI is equal to 0. Even more care should be taken
when the \emph{upper} bound is equal to 0 - this occurs when \emph{p}-value is greater
than 1-alpha/2 making, the upper bound cannot be estimated, and the upper
bound is arbitrarily set to 0 (Steiger, 2004). For example:\if{html}{\out{<div class="r">}}\preformatted{eta_squared(aov(mpg ~ factor(gear) + factor(cyl), mtcars[1:7, ]))
}\if{html}{\out{</div>}}\preformatted{## # Effect Size for ANOVA (Type I)
## 
## Parameter    | Eta2 (partial) |       90\% CI
## --------------------------------------------
## factor(gear) |           0.58 | [0.00, 0.84]
## factor(cyl)  |           0.46 | [0.00, 0.78]
}
}

\examples{

# two-sample tests -----------------------

# using formula interface
cohens_d(mpg ~ am, data = mtcars)
cohens_d(mpg ~ am, data = mtcars, pooled_sd = FALSE)
cohens_d(mpg ~ am, data = mtcars, mu = -5)
hedges_g(mpg ~ am, data = mtcars)
glass_delta(mpg ~ am, data = mtcars)
print(cohens_d(mpg ~ am, data = mtcars), append_CL = TRUE)

# other acceptable ways to specify arguments
glass_delta(sleep$extra, sleep$group)
hedges_g("extra", "group", data = sleep)
cohens_d(sleep$extra[sleep$group == 1], sleep$extra[sleep$group == 2], paired = TRUE)
# cohens_d(Pair(extra[group == 1], extra[group == 2]) ~ 1,
#          data = sleep, paired = TRUE)

# one-sample tests -----------------------

cohens_d("wt", data = mtcars, mu = 3)
hedges_g("wt", data = mtcars, mu = 3)

# interpretation -----------------------

interpret_d(0.4, rules = "cohen1988")
d_to_common_language(0.4)
interpret_g(0.4, rules = "sawilowsky2009")
interpret_delta(0.4, rules = "gignac2016")
}
\references{
\itemize{
\item Algina, J., Keselman, H. J., & Penfield, R. D. (2006). Confidence intervals
for an effect size when variances are not equal. Journal of Modern Applied
Statistical Methods, 5(1), 2.
\item Cohen, J. (1988). Statistical power analysis for the behavioral
sciences (2nd Ed.). New York: Routledge.
\item Delacre, M., Lakens, D., Ley, C., Liu, L., & Leys, C. (2021, May 7). Why
Hedges’ g*s based on the non-pooled standard deviation should be reported
with Welch's t-test. https://doi.org/10.31234/osf.io/tu6mp
\item Hedges, L. V. & Olkin, I. (1985). Statistical methods for
meta-analysis. Orlando, FL: Academic Press.
\item Hunter, J. E., & Schmidt, F. L. (2004). Methods of meta-analysis:
Correcting error and bias in research findings. Sage.
}
}
\seealso{
\code{\link[=d_to_common_language]{d_to_common_language()}} \code{\link[=sd_pooled]{sd_pooled()}}

Other effect size indices: 
\code{\link{effectsize}()},
\code{\link{eta_squared}()},
\code{\link{phi}()},
\code{\link{rank_biserial}()},
\code{\link{standardize_parameters}()}
}
\concept{effect size indices}
