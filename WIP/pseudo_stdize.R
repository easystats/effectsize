
library(dplyr)

N <- 10
k <- 6

ID <- factor(seq_len(N))
B1 <- setNames(rnorm(N),ID)
B2 <- setNames(rnorm(N),ID)
W1 <- rnorm(N*k)
W2 <- rnorm(N*k)

df <- data.frame(ID,W1,W2) %>%
  mutate(B1 = B1[ID],
         B2 = B2[ID],
         Y = rnorm(n())) %>%
  arrange(ID) %>%
  mutate(rand2 = rep(0:1, length.out = n()))



library(lme4)

model <- lmer(Y ~ B1 * W1 + B2 + W2 + (W1 + W2 | ID), data = df)
summary(model)











effectsize::standardize_info(model)






# standardize_info --------------------------------------------------------



params <- insight::find_parameters(model, effects = "fixed", flatten = TRUE)
types <- parameters::parameters_type(model)
model_matrix <- as.data.frame(stats::model.matrix(model))
data <- insight::get_data(model)

out <- data.frame(
  Parameter = params,
  Type = types$Type,
  Link = types$Link,
  Secondary_Parameter = types$Secondary_Parameter,
  stringsAsFactors = FALSE
)

# Type of effect size
out$EffectSize_Type <- ifelse(types$Type == "interaction", "interaction",
                              ifelse(types$Link == "Association", "r",
                                     ifelse(types$Link == "Difference", "d", NA)))


# ADD THIS ----------------------------------------------------------------



if (insight::model_info(model)$is_mixed &&
    length(insight::find_random(model)$random) == 1) {
  out <- merge(
    out,
    .std_info_pseudo(model, params)
  )
}





.std_info_pseudo <- function(model, params) {
  # normally, if the model is a GLM, the Y is not standardized... should this be an exception?
  if (insight::model_info(model)$is_linear) {
    # if is within:
    e <- sqrt(insight::get_variance_residual(model))

    # if between:
    # TO DO: the sqrt(variance) *explained* by the relevant random slope?
  } else {
    e <- 1
  }


  within_vars <- unclass(parameters::check_heterogeneity(model))
  id <- insight::get_random(model)[[1]]

  Deviation_Response_Pseudo <- Deviation_Pseudo <- setNames(numeric(ncol(model_matrix)),params)
  for (p in params) {
    if (p == "(Intercept)") {
      Deviation_Response_Pseudo[p] <- Deviation_Pseudo[p] <- 0
    } else if (p %in% within_vars) { # TO DO or interacts with a within var...
      # is within
      X_fit <- lme4::lmer(model_matrix[[p]] ~ 1 + (1|id))
      Deviation_Pseudo[p] <- sqrt(insight::get_variance_residual(X_fit))
    } else {
      # is between
      X <- tapply(model_matrix[[p]], id, "[", 1)
      Deviation_Pseudo[p] <- stats::sd(X)
    }
  }


  data.frame(
    Parameter = params,
    Deviation_Response_Pseudo,
    Deviation_Pseudo
  )
}

