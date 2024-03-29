% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpret_omega_squared.R
\name{interpret_omega_squared}
\alias{interpret_omega_squared}
\alias{interpret_eta_squared}
\alias{interpret_epsilon_squared}
\alias{interpret_r2_semipartial}
\title{Interpret ANOVA Effect Sizes}
\usage{
interpret_omega_squared(es, rules = "field2013", ...)

interpret_eta_squared(es, rules = "field2013", ...)

interpret_epsilon_squared(es, rules = "field2013", ...)

interpret_r2_semipartial(es, rules = "field2013", ...)
}
\arguments{
\item{es}{Value or vector of (partial) eta / omega / epsilon squared or semipartial r squared values.}

\item{rules}{Can be \code{"field2013"} (default), \code{"cohen1992"} or custom set of \code{\link[=rules]{rules()}}.}

\item{...}{Not used for now.}
}
\description{
Interpret ANOVA Effect Sizes
}
\section{Rules}{

\itemize{
\item Field (2013) (\code{"field2013"}; default)
\itemize{
\item \strong{ES < 0.01} - Very small
\item \strong{0.01 <= ES < 0.06} - Small
\item \strong{0.06 <= ES < 0.14} - Medium
\item **ES >= 0.14 ** - Large
}
\item Cohen (1992) (\code{"cohen1992"}) applicable to one-way anova, or to \emph{partial}
eta / omega / epsilon squared in multi-way anova.
\itemize{
\item \strong{ES < 0.02} - Very small
\item \strong{0.02 <= ES < 0.13} - Small
\item \strong{0.13 <= ES < 0.26} - Medium
\item \strong{ES >= 0.26} - Large
}
}
}

\examples{
interpret_eta_squared(.02)
interpret_eta_squared(c(.5, .02), rules = "cohen1992")
}
\references{
\itemize{
\item Field, A (2013) Discovering statistics using IBM SPSS Statistics. Fourth
Edition. Sage:London.
\item Cohen, J. (1992). A power primer. Psychological bulletin, 112(1), 155.
}
}
\seealso{
https://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/effectSize/
}
\keyword{interpreters}
