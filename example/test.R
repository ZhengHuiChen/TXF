## 0. =============================================================================================
# rm(list = ls())
library(TXF)
library(readr)
library(readxl)
library(tidyverse)
library(lubridate)
library(xts)
options(digits.secs = 3)

## 1. =============================================================================================
Base <- read_excel("C:/Users/zheng/Dropbox/example_7_10.xlsx",
  col_types = c(
    "date", "numeric", "numeric",
    "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric",
    "numeric", "numeric", "numeric",
    "numeric", "numeric", "text", "text",
    "text", "text", "text", "text", "text"
  )
)

dateChr <- as.character(Base[["Date"]])
nms <- c("上區", "下區", "中心", "備註")

baseTable <- lapply(X = dateChr, function(date) {
  df <- Base[str_detect(Base[["Date"]], pattern = date), ]
  l <- lapply(nms, function(chr) {
    unlist(df[, str_detect(string = names(df), pattern = chr)])
  })
  names(l) <- c("Upper", "Lower", "Center", "Notes")
  l
})
names(baseTable) <- dateChr
baseTable

baseline_list <- formBaseline(baseTable = baseTable)
# baseline_list

## 2. Data Split ==================================================================================
TXF <- read_csv("C:/Users/zheng/Dropbox/TXF1-Tick.txt",
                col_types = cols(
                  Date = col_character(),
                  Time = col_character()
                )
)

s <- seq(ISOdatetime(2018, 7, 2, 15, 0, 0), ISOdatetime(2018, 10, 30, 15, 0, 0), by = "day")
e <- seq(ISOdatetime(2018, 7, 3, 13, 44, 0), ISOdatetime(2018, 10, 31, 13, 44, 0), by = "day")

data_list <- mapply(function(tbl, s, e) {
  t <- ymd_hms(paste(tbl[["Date"]], tbl[["Time"]]), tz = "")
  tbl[t >= s & t < e, ]
},s = s,e = e,
MoreArgs = list(tbl = TXF),
SIMPLIFY = FALSE
)

n <- vapply(data_list, function(x) nrow(x), FUN.VALUE = 0)

data_list <- data_list[n != 0]
names(data_list) <- ymd(vapply(data_list, function(tbl) tbl[["Date"]][1], FUN.VALUE = character(1)))
# data_list

## 2. =============================================================================================
n <- "2018-07-03"
data <- data_list[[n]]
baseline <- baseline_list[[n]]
data[["position"]] <- 0
s <- baseline[1:(length(baseline)) - 1]
e <- baseline[2:length(baseline)]

in_point <- lapply(X = data$Price, function(x) {
  mapply(dplyr::between, left = s, right = e, MoreArgs = list(x = x), SIMPLIFY = TRUE)
  }
)
names(in_point) <- data$Time

enter <- vapply(in_point, which.max, FUN.VALUE = 1)

data$position







for (i in seq_along(data$Price)) {
  l <- 1
  r <- length(baseline)
  while (between(x = data$Price[i], left = baseline[l], right = baseline[r])) {
    l <- l + 1
    r <- r - 1
  }
  print(l)
  print(r)
  # print(p)
}

















