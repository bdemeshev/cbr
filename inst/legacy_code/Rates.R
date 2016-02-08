

Sys.getlocale()  # LC_COLLATE=Russian_Russia.1251
Sys.setlocale(category = "LC_ALL", locale = "Russian_Russia.1251")
Sys.setlocale(category = "LC_ALL", locale = "C")
start.date <- as.Date("2013-01-01")
end.date <- as.Date("2014-04-29")



saldo <- SaldoXML(start.date, end.date)
Ruonia <- RuoniaXML(start.date, end.date)
Depo <- DepoDynamicXML(start.date, end.date)
Repo <- RepoSessionXML(start.date, end.date)

miacr <- MKRXML(start.date, end.date)
miacr <- subset(miacr, miacr$p1 == 3, select = c("CDate", "d1"))  #выделить только MIACR и овернайт
miacr <- xts(miacr[, -1], order.by = miacr[, 1])


mosprime <- MosPrimeXML(start.date, end.date)
tail(mosprime)


# Аукционы РЕПО по перемеменной ставке
repo.v <- REPOXML(start.date, end.date)
repo.1 <- subset(repo.v, day_repo == 1)
repo.7 <- subset(repo.v, day_repo == 7)

repo.7 <- repo.7[, c("Dt", "avg_deal", "avg_yield")]
repo.7 <- as.xts(repo.7[, -1], order.by = repo.7[, 1])
a <- apply.daily(repo.7[, "avg_yield"], mean)
b <- apply.daily(repo.7[, "avg_deal"], sum)
repo.7 <- merge(a, b)
par(mfrow = c(2, 1))
chart.TimeSeries.IEF(repo.7[, 1])
chart.TimeSeries.IEF(repo.7[, 2]/1000)
par(mfrow = c(1, 1))

repo.1 <- repo.1[, c("Dt", "avg_deal", "avg_yield")]
repo.1 <- as.xts(repo.1[, -1], order.by = repo.1[, 1])
a <- apply.daily(repo.1[, "avg_yield"], mean)
b <- apply.daily(repo.1[, "avg_deal"], sum)
repo.1 <- merge(a, b)
par(mfrow = c(1, 1))
chart.TimeSeries.IEF(repo.1[, 1])
chart.TimeSeries.IEF(repo.1[, 2]/1000)
repo.volume <<- na.locf(merge(repo.1[, 2], repo.7[, 2]))
names(repo.volume) <- c("1 day", "7days")
repo.volume <- na.omit(repo.volume)
chart.StackedBar(repo.volume/1000, las = 1, colorset = ief.palette, border = NA,
  minor.ticks = FALSE, ylab = "bln RUB")
openGraph(width = 7, height = 4)
saveGraph(file = "graph/repo-auction-volumes.emf", type = "emf")


repo.yield <<- na.locf(merge(repo.1[, 1], repo.7[, 1]))
names(repo.yield) <- c("1 day", "7days")
repo.yield <- na.omit(repo.yield)
chart.TimeSeries.IEF(repo.yield, legend.loc = "top")


aa <- by(repo.v[, c("avg_yield")], repo.v[, "Dt"], FUN = median)
repo.ir <- do.call(rbind, as.list(aa))
repo.ir <- xts(repo.ir, order.by = as.Date(row.names(repo.ir)))
repo.ir <- repo.ir[!(repo.ir == 0)]


unsec <- UnSecLoansXML(start.date, end.date)

nonmarket <- NonMarketCreditXML(start.date, end.date)



buffer <- na.locf(merge.xts(Ruonia$ruo, Depo$TomNext, Repo$Rate7Day, mosprime$T3M,
  repo.ir))
buffer <- na.locf(buffer, fromLast = TRUE)
names(buffer) <- c("RUONIA", "Standing depo", "Standing repo (7 day)",
  "Mosprime 3m", "Auction repo (7 days)")
tail(buffer)

openGraph(width = 8, height = 3.4)
par(bty = "l")  #  глобальный параметр - граница вокруг графика слева и снизу
par(las = 1)  #  глобальный параметр - текст пишется параллельно оси X
par(mar = c(2, 3, 1, 1))  #  глобальный параметр - границы между область графика и остальной частью

chart.TimeSeries.IEF(buffer, date.format = "%b-%Y", main = "", colorset = ief.palette,
  legend.loc = "topleft", ylab = "%", xlab = "")

saveGraph(file = "graph/interest_rates", type = "emf")
dev.off()

RosneftChart(buffer)


tail(saldo)
gg <- merge(saldo, Ruonia$ruo)
gg <- gg["2012::"]
Plot2.IEF(saldo, Ruonia$ruo, name1 = "Liquidity saldo", name2 = "RUONIA",
  ylab1 = "bln RUB")

dv <- DVXML(start.date, end.date)
