
# RCT ---------------------------------------------------------------------

RCT_table <- matrix(c(
  71, 50,
  30, 100
), nrow = 2)
dimnames(RCT_table) <- list(
  Diagnosis = c("Sick", "Recovered"),
  Group = c("Treatment", "Control")
)
RCT_table <- as.table(RCT_table)

save(RCT_table, file = "data/RCT_table.rdata")


# Music -------------------------------------------------------------------

Music_preferences <- matrix(c(
  150, 100, 165,
  130, 50, 65,
  35, 10, 2,
  55, 40, 25
), nrow = 4)
dimnames(Music_preferences) <- list(
  c("Pop", "Rock", "Jazz", "Classic"),
  c("Psych", "Econ", "Law")
)
Music_preferences <- as.table(Music_preferences)

save(Music_preferences, file = "data/Music_preferences.rdata")

# FASD ---------------------------------------------------------------------

Smoking_FASD <- as.table(c(FAS = 17, PFAS = 11, TD = 640))

save(Smoking_FASD, file = "data/Smoking_FASD.rdata")
