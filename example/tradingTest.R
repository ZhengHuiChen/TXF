## trading test ===================================================================================
# rm(list = ls())
exa <- data_list[[2]]
exa_date <- names(data_list)[2]

exa
exa_date

baseLineEx <- Baseline[[2]]
baseLineEx_date <- names(Baseline)[2]

baseLineEx
baseLineEx_date

## 1.0 Draft =====================================================================================
datetimes <- ymd_hms(paste(exa$Date, exa$Time), tz = "")
prices <- exa$Price

baseLineEx <- c(0, baseLineEx, Inf)
ind <- cut(prices, breaks = baseLineEx, labels = FALSE)

ceilings <- baseLineEx[ind + 1]
floors <- baseLineEx[ind]

