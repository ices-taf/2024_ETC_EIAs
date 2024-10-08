calc_scores <- function(file) {
  data <- read.csv(file)

  data <- data[, 1:6]
  names(data)[1:6] <-
    c(
      "Sector", "Pressure", "Ecological.Characteristic",
      "Overlap", "Frequency", "DoI"
    )

  ## calculate the scores (include as input data)
  scores <- list(
    Overlap = c(WE = 1, WP = 0.67, L = 0.33, S = 0.03, E = 0.01),
    Frequency = c(P = 1, C = 0.67, O = 0.33, R = 0.08),
    DoI = c(A = 1, C = 0.2, L = 0.05),
    confidence =
      c(
        "1a" = "No Specific Expertise", "1b" = "Specific Expertise",
        "2a" = "Global Literature", "2b" = "Regional Literature",
        "3" = "Data Regional - Monitoring"
      )
  )

  calc.score <- function(what) {
    unname(scores[[what]][data[[what]]])
  }

  data$Overlap.Score <- calc.score("Overlap")
  data$Frequency.Score <- calc.score("Frequency")
  data$DoI.Score <- calc.score("DoI")

  ## Calculate Impact Risk, and log IR (for figure later)
  data$ImpactRisk <- data$Overlap.Score * data$Frequency.Score * data$DoI.Score
  data$LN.IR <- log(data$ImpactRisk)

  data <- data[complete.cases(data), ]

  data
}
