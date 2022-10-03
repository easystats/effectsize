

# Data frames -------------------------------------------------------------

#' Workers' Salary and Other Information
#'
#' A sample (simulated) dataset, used in tests and some examples.
#'
#' @docType data
#'
#' @name hardlyworking
#'
#' @keywords data
#'
#' @format A data frame with 500 rows and 5 variables:
#' \describe{
#'   \item{salary}{Salary, in Shmekels}
#'   \item{xtra_hours}{Number of overtime hours (on average, per week)}
#'   \item{n_comps}{Number of compliments given to the boss (observed over the last week)}
#'   \item{age}{Age in years}
#'   \item{seniority}{How many years with the company}
#'   \item{is_senior}{Has this person been working here for more than 4 years?}
#' }
#'
#' ```{r}
#' data("hardlyworking")
#' head(hardlyworking, n = 5)
#' ```
#' @family effect size datasets
#'
NULL

#' Results from 2 Screening Tests
#'
#' A sample (simulated) dataset, used in tests and some examples.
#'
#' @docType data
#'
#' @name screening_test
#'
#' @keywords data
#'
#' @format A data frame with 1600 rows and 3 variables:
#' \describe{
#'   \item{Diagnosis}{Ground truth}
#'   \item{Test1}{Results given by the 1st test}
#'   \item{Test2}{Results given by the 2nd test}
#' }
#' ```{r}
#' data("screening_test")
#' head(screening_test, n = 5)
#' ```
#'
#' @family effect size datasets
#'
NULL


# Tables ------------------------------------------------------------------



#' Fictional Results from a Workers' Randomized Control Trial
#'
#' @docType data
#'
#' @name RCT_table
#'
#' @keywords data
#'
#' @format A 2-by-2 table, with a *column* for each group and a *row* for the diagnosis.
#'
#' ```{r}
#' data("RCT_table")
#' RCT_table
#' ```
#'
#' @family effect size datasets
#'
NULL


#' Music Preference by College Major
#'
#' Fictional data.
#'
#' @docType data
#'
#' @name Music_preferences
#'
#' @keywords data
#'
#' @format A 4-by-3 table, with a *column* for each major and a *row* for each type of music.
#'
#' ```{r}
#' data("Music_preferences")
#' Music_preferences
#' ```
#'
#' @family effect size datasets
#'
NULL


#' Music Preference by College Major
#'
#' Fictional data, with more extreme preferences than [Music_preferences]
#'
#' @docType data
#'
#' @name Music_preferences2
#'
#' @keywords data
#'
#' @format A 4-by-3 table, with a *column* for each major and a *row* for each type of music.
#'
#' ```{r}
#' data("Music_preferences2")
#' Music_preferences2
#' ```
#'
#' @family effect size datasets
#'
NULL

#' Frequency of FASD for Smoking Mothers
#'
#' Fictional data.
#'
#' @docType data
#'
#' @name Smoking_FASD
#'
#' @keywords data
#'
#' @format A 1-by-3 table, with a *column* for each diagnosis.
#'
#' ```{r}
#' data("Smoking_FASD")
#' Smoking_FASD
#' ```
#'
#' @family effect size datasets
#'
NULL


#' Classification of Foods
#'
#' Fictional data.
#'
#' @docType data
#'
#' @name food_class
#'
#' @keywords data
#'
#' @format A 2-by-3 table.
#'
#' ```{r}
#' data("food_class")
#' food_class
#' ```
#'
#' @family effect size datasets
#'
NULL
