library(effectsize)
library(correlation)


# Table 1

(Titanic_xtab <- as.table(apply(Titanic, c(2, 4), sum)))
chisq.test(Titanic_xtab)


# Table 2

phi(Titanic_xtab, adjust = FALSE)

tidyr::uncount(as.data.frame(Titanic_xtab), weights = Freq) |>
  transform(Survived = Survived == "Yes", Sex = Sex == "Male") |>
  correlation()


# Table 3

(Titanic_xtab2 <- as.table(apply(Titanic, c(1, 4), sum)))

cramers_v(Titanic_xtab2, adjust = FALSE)


# Table 4

data("food_class")
food_class
cramers_v(food_class, adjust = FALSE)
tschuprows_t(food_class)


# Table 5

O <- c(90, 10)
p_E <- c(0.5, 0.5)
cohens_w(O, p = p_E)

p_E <- c(0.35, 0.65)
cohens_w(O, p = p_E)

O <- c(5, 10, 80, 5)
p_E <- c(0.25, 0.25, 0.25, 0.25)
cohens_w(O, p = p_E)


# Table 6

O <- c(90, 10)
p_E <- c(0.5, 0.5)
fei(O, p = p_E)

p_E <- c(0.35, 0.65)
fei(O, p = p_E)

O <- c(5, 10, 80, 5)
p_E <- c(0.25, 0.25, 0.25, 0.25)
fei(O, p = p_E)
