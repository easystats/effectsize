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

#' Jeff Rouder's Example Dataset for Repeated Measures
#'
#' A dataset "with 25 people each observing 50 trials in 2 conditions",
#' published as `effectSizePuzzler.txt` by Jeff Rouder on March 24, 2016
#' (_http://jeffrouder.blogspot.com/2016/03/the-effect-size-puzzler.html_).
#' \cr\cr
#' The data is used in examples and tests of [rm_d()].
#'
#' @docType data
#'
#' @name rouder2016
#'
#' @keywords data
#'
#' @format A data frame with 2500 rows and 3 variables:
#' \describe{
#'   \item{id}{participant: 1...25}
#'   \item{cond}{condition: 1,2}
#'   \item{rt}{response time in seconds}
#' }
#'
#' ```{r}
#' data("rouder2016")
#' head(rouder2016, n = 5)
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


#' Preferences of Poop vs Chocolate
#'
#' A subset of Hussey and Cummins replication of _Balcetis & Dunning (2010)
#' Study 3b_. Each scale is average of 3 items both for "a toilet filled with
#' human poop" and for  and for "a toilet filled with human poop" (how positive
#' or negative / pleasant or unpleasant / good or bad).
#'
#' @references  Hussey, I., & Cummins, J. (2025). (Not so) simple preferences. https://github.com/ianhussey/not-so-simple-preferences
#'
#' @docType data
#'
#' @name preferences2025
#'
#' @keywords data
#'
#' @format A data frame with 489 rows and 3 variables:
#' \describe{
#'   \item{participant_id}{Unique identifier for each participant}
#'   \item{poop}{Preference rating of poop (1 = low, 7 = high)}
#'   \item{chocolate}{Preference rating of poop (1 = low, 7 = high)}
#' }
#'
#' ```{r}
#' data("preferences2025")
#' head(preferences2025, n = 5)
#' ```
#' @family effect size datasets
#'
#' @seealso [repeated_measures_d()]
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
