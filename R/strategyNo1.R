strategyNo1 <- function(Baseline, R, m = 1, tolerance = 1) {
  l <- list(
    list(
      Baseline = Baseline,
      `Baseline + R` = Baseline + m * R, ## long
      `Baseline - R` = Baseline - m * R ## short
    ),
    list(
      `Baseline + tolerance` = Baseline + tolerance, ## long
      `Baseline + R + tolerance` = Baseline + m * R + tolerance, ## long
      `Baseline - R + tolerance` = Baseline - m * R + tolerance ## long
    ),
    list(
      `Baseline - tolerance` = Baseline - tolerance, ## short
      `Baseline + R - tolerance` = Baseline + m * R - tolerance, ## short
      `Baseline - R - tolerance` = Baseline - m * R - tolerance ## short
    )
  )
  names(l) <- c("Base", "plusTolerance", "minustolerance")
  benchmark <- sort(unlist(l[["Base"]]), decreasing = TRUE)
}
