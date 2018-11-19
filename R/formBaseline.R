formBaseline <- function(baseTable, decreasing = FALSE) {
  lapply(baseTable, function(x) {
    p <- sort(unlist(x[setdiff(x = names(x), y = "Notes")], use.names = FALSE),
              decreasing = FALSE)
    names(p) <- as.character(p)
    a <- str_split(x[["Notes"]], pattern = "\\+-")
    ind <- vapply(a, function(x) {
      all(Negate(is.na)(x))
    },
    FUN.VALUE = logical(1))
    a <- a[ind]
    r <- as.numeric(a[[1]])
    temp <- vapply(a[-1], function(x) {
      as.numeric(x[2])
    }, 0)
    names(temp) <- vapply(a[-1], function(x) {
      x[1]
    }, FUN.VALUE = character(1))

    p1 <- p[setdiff(names(p), y = names(temp))] + r
    p2 <- p[setdiff(names(p), y = names(temp))] - r
    specials <- vapply(names(temp), function(n) {
      c(
        p[n] + temp[n],
        p[n] - temp[n]
      )
    }, FUN.VALUE = numeric(2))
    result <- sort(unlist(c(p1, p2, specials), use.names = FALSE), decreasing = decreasing)
    names(result) <- NULL
    result
  })
}

