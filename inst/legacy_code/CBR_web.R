
# Салихов Марсель (quantviews.blogspot.ru)
library(XML)
library(RCurl)
library(SSOAP)
library(xts)
library(ggplot2)
library(dplyr)
library(scales)
library(reshape2)
library(RColorBrewer)
library(quantmod)
library(PerformanceAnalytics)
# library(dplyr)

# установка пакета SSOAP, если необходимо install.packages('SSOAP',
# repos = 'http://www.omegahat.org/R', dependencies = TRUE, type =
# 'source')


# Генерическая функция обращения к разделу Банка России для получения
# информации по рынку ценных бумаг
# http://cbr.ru/scripts/Root.asp?Prtid=SEC
SecFunction <- function(name, DateFrom, DateTo) {
  h <- basicTextGatherer()  #фунция для обработки http-запросов
  url <- "http://cbr.ru/secinfo/secinfo.asmx"
  # сформировать тело SOAP запроса
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <",
    name, " xmlns=\"http://web.cbr.ru/\">\n                <DateFrom>",
    DateFrom, "</DateFrom>\n                <DateTo>", DateTo, "</DateTo>\n                </",
    name, ">\n                </soap:Body>\n                </soap:Envelope>")
  HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*", SOAPAction = paste("\"http://web.cbr.ru/",
    name, "\"", sep = ""), `Content-Type` = "text/xml; charset=utf-8")
  curlPerform(url = url, httpheader = HeaderFields, postfields = body,
    writefunction = h$update)
  response <- h$value()  #получение ответа от сервера
  doc <- xmlInternalTreeParse(response)  # создание XML-дерева
  return(doc)

}

SecFunction2 <- function(name, OnDate, ToDate) {
  h <- basicTextGatherer()  #фунция для обработки http-запросов
  url <- "http://cbr.ru/secinfo/secinfo.asmx"
  # сформировать тело SOAP запроса
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <",
    name, " xmlns=\"http://web.cbr.ru/\">\n                <OnDate>",
    OnDate, "</OnDate>\n                <ToDate>", ToDate, "</ToDate>\n                </",
    name, ">\n                </soap:Body>\n                </soap:Envelope>")
  HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*", SOAPAction = paste("\"http://web.cbr.ru/",
    name, "\"", sep = ""), `Content-Type` = "text/xml; charset=utf-8")
  curlPerform(url = url, httpheader = HeaderFields, postfields = body,
    writefunction = h$update)
  response <- h$value()  #получение ответа от сервера
  doc <- xmlInternalTreeParse(response)  # создание XML-дерева
  return(doc)

}

SecFunction3 <- function(name, dt) {
  h <- basicTextGatherer()  #фунция для обработки http-запросов
  url <- "http://cbr.ru/secinfo/secinfo.asmx"
  # сформировать тело SOAP запроса
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <",
    name, " xmlns=\"http://web.cbr.ru/\">\n                <dt>", dt,
    "</dt>\n                \n                </", name, ">\n                </soap:Body>\n                </soap:Envelope>")
  HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*", SOAPAction = paste("\"http://web.cbr.ru/",
    name, "\"", sep = ""), `Content-Type` = "text/xml; charset=utf-8")
  curlPerform(url = url, httpheader = HeaderFields, postfields = body,
    writefunction = h$update)
  response <- h$value()  #получение ответа от сервера
  doc <- xmlInternalTreeParse(response)  # создание XML-дерева
  return(doc)

}


SecFunction4 <- function(name, dt) {
  h <- basicTextGatherer()  #фунция для обработки http-запросов
  url <- "http://cbr.ru/secinfo/secinfo.asmx"
  # сформировать тело SOAP запроса
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <",
    name, " xmlns=\"http://web.cbr.ru/\">\n                <OnDate>",
    dt, "</OnDate>\n                \n                </", name, ">\n                </soap:Body>\n                </soap:Envelope>")
  HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*", SOAPAction = paste("\"http://web.cbr.ru/",
    name, "\"", sep = ""), `Content-Type` = "text/xml; charset=utf-8")
  curlPerform(url = url, httpheader = HeaderFields, postfields = body,
    writefunction = h$update)
  response <- h$value()  #получение ответа от сервера
  doc <- xmlInternalTreeParse(response)  # создание XML-дерева
  return(doc)

}

# Генерическая функция обращения к разделу Банка России 'Веб - сервис
# для получения ежедневных данных'г
# http://cbr.ru/scripts/Root.asp?Prtid=DWS

DailyFunction <- function(name, fromDate, ToDate) {
  h <- basicTextGatherer()  #фунция для обработки http-запросов
  url <- "http://www.cbr.ru/DailyInfoWebServ/DailyInfo.asmx"
  # сформировать тело SOAP запроса
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <",
    name, " xmlns=\"http://web.cbr.ru/\">\n                <fromDate>",
    fromDate, "</fromDate>\n                <ToDate>", ToDate, "</ToDate>\n                </",
    name, ">\n                </soap:Body>\n                </soap:Envelope>")
  HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*", SOAPAction = paste("\"http://web.cbr.ru/",
    name, "\"", sep = ""), `Content-Type` = "text/xml; charset=utf-8")
  curlPerform(url = url, httpheader = HeaderFields, postfields = body,
    writefunction = h$update)
  response <- h$value()  #получение ответа от сервера
  doc <- xmlInternalTreeParse(response)  # создание XML-дерева
  return(doc)

}


# Функция преобразования в результатов генерических функций Daily и Sec
# в итоговый датафрейм аргументы: doc - XML-дерево
# (xmlInternalTreeParse) : 'headtag - тег первого уровня в ответе SOAP
Doc2Df <- function(doc, headtag) {
  cleanup <- getNodeSet(doc, paste0("//", headtag))  # создаем список, разбивая по headtag
  vars <- names(cleanup[[length(cleanup)]])  # имена переменных. Использует последняя точка для получения названий
  dd <- lapply(cleanup, function(x) {
    # применить преобразования к каждому элементу списка cleanup
    a <- getNodeSet(x, vars)
    names <- sapply(a, xmlName)  # выделить названия переменных - атрибуты ячеек xml
    values <- sapply(a, xmlValue)  # выделить значения переменных
    names(values) <- names
    nots <- vars[!(vars %in% names(values))]  # если какие-то ячейки отсутствуют в данной ноде
    values[nots] <- NA  # то ставим NA
    return(values)
  })
  df <- as.data.frame(do.call(rbind, dd))
  return(df)


}


# Задолженность кредитных организаций перед Банком России по операциям
# РЕПО в иностранной валюте (XMLDocument)
RepoDebtUSDXML <- function(fromDate, ToDate) {
  doc <- DailyFunction("RepoDebtUSDXML", fromDate, ToDate)
  df <- Doc2Df(doc, "rd")
  df[, "D0"] <- as.Date(as.POSIXct(df[, "D0"])) + 1
  df <- xts(apply(select(df, -D0), 2, as.numeric), order.by = df$D0)
  return(df)

}



# Срочная структура процентных ставок (кривая бескупонной доходности)
# (как DataSet)
GCurve <- function(onDate) {
  doc <- SecFunction4("GCurve", onDate)
  df <- Doc2Df(doc, "GC")
  names(df)[1] <- "year"
  df <- as.data.frame(apply(df, 2, as.numeric))
  return(df)
}

Isoterm <- function(onDate, ToDate, I_Day) {
  if (!onDate < ToDate) {
    stop("Dates sequence error")
  }
  name <- "Isoterm"
  h <- basicTextGatherer()  #фунция для обработки http-запросов
  url <- "http://cbr.ru/secinfo/secinfo.asmx"
  # сформировать тело SOAP запроса
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n                <soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <",
    name, " xmlns=\"http://web.cbr.ru/\">\n                <OnDate>",
    OnDate, "</OnDate>\n                <ToDate>", ToDate, "</ToDate>\n                <I_Day>",
    I_Day, "</I_Day>\n                </", name, ">\n                </soap:Body>\n                </soap:Envelope>")
  HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*", SOAPAction = paste("\"http://web.cbr.ru/",
    name, "\"", sep = ""), `Content-Type` = "text/xml; charset=utf-8")
  curlPerform(url = url, httpheader = HeaderFields, postfields = body,
    writefunction = h$update)
  response <- h$value()  #получение ответа от сервера
  doc <- xmlInternalTreeParse(response)  # создание XML-дерева
  df <- Doc2Df(doc, "It")
  df[, "day"] <- as.Date(as.POSIXct(df[, "day"])) + 1
  df$val <- as.numeric(as.character(df$val))
  df <- xts(df$val, order.by = df$day)
  return(df)
}

# Итоги аукциона прямого РЕПО в иностранной валюте (как XMLDocument)
REPOFXXML <- function(DateFrom, DateTo) {
  doc <- SecFunction("REPOFXXML", DateFrom, DateTo)
  df <- Doc2Df(doc, "REPOFX/RP")
  df[, "first_date"] <- as.Date(as.POSIXct(df[, "first_date"])) + 1
  df[, "snd_date"] <- as.Date(as.POSIXct(df[, "snd_date"])) + 1
  df[, "Dt"] <- as.Date(as.POSIXct(df[, "Dt"])) + 1
  df[, c("avg_deal", "avg_yield", "day_repo", "bid", "max_state_intrate",
    "min_state_intrate", "cut_off_rate", "avg_deal_lim", "avg_yield_lim")] <- apply(df[,
    c("avg_deal", "avg_yield", "day_repo", "bid", "max_state_intrate",
      "min_state_intrate", "cut_off_rate", "avg_deal_lim", "avg_yield_lim")],
    2, as.numeric)
  return(df)
}

# Условия проведения Банком России операций по предоставлению кредитным
# организациям обеспеченных кредитов Банка России по фиксированным
# процентным ставкам (кроме кредитов овернайт)
creditco <- function(dt) {
  doc <- SecFunction3("creditco", dt)

}

# Параметры курсовой политики Банка России (как XMLDocument)
Sp_fxpm_XML <- function(fromDate, DateTo) {
  doc <- SecFunction("Sp_fxpm_XML", fromDate, DateTo)
  df <- Doc2Df(doc, "Data/Corridor")
  df1 <- Doc2Df(doc, "Data/SmoothingInterventions")
  df2 <- Doc2Df(doc, "Data/InternalRangesWidth")
  df3 <- as.data.frame(sapply(getNodeSet(doc, paste0("//", "D0")), xmlValue))
  names(df3) <- "D0"
  df4 <- as.data.frame(sapply(getNodeSet(doc, paste0("//", "CumulativeVolume")),
    xmlValue))
  names(df4) <- "CumulativeVolume"
  df5 <- as.data.frame(sapply(getNodeSet(doc, paste0("//", "TreasuryOrderVolulme")),
    xmlValue))
  names(df5) <- "TreasuryOrderVolulme"
  df <- cbind(df, df1, df2, df3, df4, df5)

  df[, "D0"] <- as.Date(as.POSIXct(df[, "D0"])) + 1
  dt <- as.Date(as.POSIXct(df[, "D0"])) + 1
  df$D0 <- NULL
  df <- apply(df, 2, as.numeric)
  df <- xts(df, order.by = dt)
  return(df)


}


# Ставки межбанковского кредитного рынка (как xmlDocument)
MKRXML <- function(fromDate, ToDate) {
  doc <- DailyFunction("MKRXML", fromDate, ToDate)
  df <- Doc2Df(doc, "MKR")
  df[, "CDate"] <- as.Date(as.POSIXct(df[, "CDate"])) + 1
  df[, "p1"] <- as.factor(df[, "p1"])
  # p1 :тип 1-MIBID, 2-MIBOR, 3-MIACR, 4-MIACR-IG, 5-MIACR-B,
  # 6-MIACR(оборот), 7-MIACR-IG(оборот), 8-MIACR-B(оборот)
  df[, 3:length(names(df))] <- apply(df[, 3:length(names(df))], 2, as.numeric)  #преобразовать остальные столбцы в числа
  return(df)
}

# Функция DepoDynamicXML
# http://cbr.ru/DailyInfoWebServ/DailyInfo.asmx?op=DepoDynamicXML
# Динамики ставок привлечения средств по депозитным операциям (как
# xmlDocument

DepoDynamicXML <- function(fromDate, ToDate) {
  doc <- DailyFunction("DepoDynamicXML", fromDate, ToDate)
  df <- Doc2Df(doc, "Depo")
  df[, "DateDepo"] <- as.Date(as.POSIXct(df[, "DateDepo"])) + 1
  df[, 2:length(names(df))] <- apply(df[, 2:length(names(df))], 2, as.numeric)  #преобразовать остальные столбцы в числа
  df <- xts((df[, -1]), order.by = df[, 1])
  return(df)

}

# Сессия прямого РЕПО по фиксированной ставке (как XMLDocument)
RepoSessionXML <- function(DateFrom, DateTo) {
  doc <- SecFunction("RepoSessionXML", DateFrom, DateTo)
  df <- Doc2Df(doc, "RS")
  df[, "D0"] <- as.Date(as.POSIXct(df[, "D0"])) + 1
  df[, 2:length(names(df))] <- apply(df[, 2:length(names(df))], 2, as.numeric)
  df <- xts((df[, -1]), order.by = df[, 1])
  return(df)

}

# Параметры аукционов прямого РЕПО (как XMLDocument)
DirRepoAuctionParamXML <- function(OnDate, ToDate) {
  doc <- SecFunction2("DirRepoAuctionParamXML", OnDate, ToDate)
  df <- Doc2Df(doc, "DR")
  df[, "D0"] <- as.Date(as.POSIXct(df[, "D0"])) + 1
  df <- xts((df[, -1]), order.by = df[, 1])
  return(df)

}

# MosPrime Rate (как XMLDocument)
MosPrimeXML <- function(DateFrom, DateTo) {
  doc <- SecFunction("MosPrimeXML", DateFrom, DateTo)
  df <- Doc2Df(doc, "MP")
  df[, "MP_Date"] <- as.Date(as.POSIXct(df[, "MP_Date"])) + 1
  df[, 2:length(names(df))] <- apply(df[, 2:length(names(df))], 2, as.numeric)
  df <- xts((df[, -1]), order.by = df[, 1])
  return(df)
}

# Итоги аукциона прямого РЕПО (как XMLDocument) <xs:element name='Dt'
# msdata:Caption='Дата' type='xs:dateTime'/> <xs:element
# name='avg_deal' msdata:Caption='Общий объем заключенных сделок, млн.'
# type='xs:decimal' minOccurs='0'/> <xs:element name='avg_yield'
# msdata:Caption='Средневзвешенная ставка, % годовых' type='xs:decimal'
# minOccurs='0'/> <xs:element name='snd_date' msdata:Caption='Дата
# исполнения второй части РЕПО' type='xs:dateTime' minOccurs='0'/>
# <xs:element name='day_repo' msdata:Caption='Срок РЕПО, дни'
# type='xs:decimal' minOccurs='0'/> <xs:element name='mTime'
# msdata:Caption='время аукциона' type='xs:string'/> <xs:element
# name='PCode' msdata:Caption='Код расчетов по сделкам'
# type='xs:string' minOccurs='0'/> <xs:element name='bid'
# msdata:Caption='Объем спроса, млн. руб.' type='xs:decimal'
# minOccurs='0'/> <xs:element name='max_state_intrate'
# msdata:Caption='Максимальная заявленная ставка, % годовых'
# type='xs:decimal' minOccurs='0'/> <xs:element
# name='min_state_intrate' msdata:Caption='Минимальная заявленная
# ставка, % годовых' type='xs:decimal' minOccurs='0'/> <xs:element
# name='cut_off_rate' msdata:Caption='Ставка отсечения, % годовых'
# type='xs:decimal' minOccurs='0'/> <xs:element name='avg_deal_lim'
# msdata:Caption='Объем заключенных сделок в рамках лимита, млн. руб.'
# type='xs:decimal' minOccurs='0'/> <xs:element name='avg_yield_lim'
# msdata:Caption='Средневзвешенная ставка по заявкам, удовлетворенным в
# рамках лимита, % годовых' type='xs:decimal' minOccurs='0'/>
# <xs:element name='first_date' msdata:Caption='Дата исполнения первой
# части сделки РЕПО' type='xs:dateTime' minOccurs='0'/> </xs:sequence>

REPOXML <- function(DateFrom, DateTo) {
  if (!DateFrom < DateTo) {
    stop("Dates sequence error")
  }

  doc <- SecFunction("REPOXML", DateFrom, DateTo)
  df <- Doc2Df(doc, "RP")
  df[, "Dt"] <- as.Date(as.POSIXct(df[, "Dt"])) + 1
  df[, "snd_date"] <- as.Date(as.POSIXct(df[, "snd_date"])) + 1
  df[, "first_date"] <- as.Date(as.POSIXct(df[, "first_date"])) + 1
  num <- c("avg_deal", "avg_yield", "bid", "max_state_intrate", "min_state_intrate",
    "cut_off_rate", "avg_deal_lim", "avg_yield_lim")
  df[, num] <- apply(df[, num], 2, as.numeric)
  return(df)

}

# Информация по аукционам ГКО-ОФЗ (как XMLDocument) !!! только по 1
# января 2013 года

AuctionsXML <- function(DateFrom, DateTo) {
  h <- basicTextGatherer()  #фунция для обработки http-запросов
  url <- "http://cbr.ru/secinfo/secinfo.asmx"
  # сформировать тело SOAP запроса
  body <- paste("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <AuctionsXML xmlns=\"http://web.cbr.ru/\">\n                <DateFrom>",
    DateFrom, "</DateFrom>\n                <DateTo>", DateTo, "</DateTo>\n                </AuctionsXML>\n                </soap:Body>\n                </soap:Envelope>")


  HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*", SOAPAction = "\"http://web.cbr.ru/AuctionsXML\"",
    `Content-Type` = "text/xml; charset=utf-8")
  curlPerform(url = url, httpheader = HeaderFields, postfields = body,
    writefunction = h$update)
  response <- h$value()  #получение ответа от сервера
  doc <- xmlInternalTreeParse(response)  # создание XML-дерева
  cleanup <- getNodeSet(doc, "//AU")  #выделение ячеек с данными
  vars <- names(cleanup[[1]])
  df <- as.data.frame(matrix(nrow = length(cleanup), ncol = length(vars),
    dimnames = list(NULL, vars)))
  a <- cleanup[[1]]
  for (j in 1:length(cleanup)) {
    # по всем строкам
    a <- cleanup[[j]]  # выделить отдельную ячейку и пройтись по ней
    for (i in 1:length(vars)) {
      cur <- vars[i]
      cc <- getNodeSet(a, cur)  #получить список значений XML ячейки
      drow <- (sapply(cc, xmlValue))  # преобразовать список в вектор
      if (length(drow) > 0)
        df[j, i] <- drow else df[j, i] <- NA  #если нет каких-то значений, то поставить NA
    }
    # print(j)
  }
  # преобразовать в даты
  df[, "d0"] <- as.Date(as.POSIXct(df[, "d0"])) + 1
  df[, "BO_FINISH"] <- as.Date(as.POSIXct(df[, "BO_FINISH"])) + 1
  df[, 7:19] <- apply(df[, 7:19], 2, as.numeric)
  return(df)

}

# База данных по купонным выплатам и погашениям (как XMLDocument)
# Cхема: http://www.cbr.ru/scripts/SecInfo_XSD/Coupons.xsd
CouponsXML <- function(DateFrom, DateTo) {
  if (!DateFrom < DateTo) {
    stop("Dates sequence error")
  }
  doc <- SecFunction("CouponsXML", DateFrom, DateTo)
  df <- Doc2Df(doc, "CP")
  h <- basicTextGatherer()  #фунция для обработки http-запросов
  url <- "http://cbr.ru/secinfo/secinfo.asmx"
  # сформировать тело SOAP запроса
  body <- paste("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <CouponsXML xmlns=\"http://web.cbr.ru/\">\n                <DateFrom>",
    DateFrom, "</DateFrom>\n                <DateTo>", DateTo, "</DateTo>\n                </CouponsXML>\n                </soap:Body>\n                </soap:Envelope>")

  HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*", SOAPAction = "\"http://web.cbr.ru/CouponsXML\"",
    `Content-Type` = "text/xml; charset=utf-8")
  curlPerform(url = url, httpheader = HeaderFields, postfields = body,
    writefunction = h$update)
  response <- h$value()  #получение ответа от сервера
  doc <- xmlInternalTreeParse(response)  # создание XML-дерева
  cleanup <- getNodeSet(doc, "//CP")  #выделение ячеек с данными
  vars <- names(cleanup[[1]])  # получить заголовки переменыными
  # сформировать датафрейм исходя из количества переменных и их их длины
  df <- as.data.frame(matrix(nrow = length(cleanup), ncol = length(vars),
    dimnames = list(NULL, vars)))
  # пройтись по всем переменным в цикле (вложенный цикл необходим так в
  # некоторых элементах есть не все ячейки)
  a <- cleanup[[1]]
  for (j in 1:length(cleanup)) {
    # по всем строкам
    a <- cleanup[[j]]  # выделить отдельную ячейку и пройтись по ней
    for (i in 1:length(vars)) {
      cur <- vars[i]
      cc <- getNodeSet(a, cur)  #получить список значений XML ячейки
      drow <- (sapply(cc, xmlValue))  # преобразовать список в вектор
      if (length(drow) > 0)
        df[j, i] <- drow else df[j, i] <- NA  #если нет каких-то значений, то поставить NA
    }
    # print(j)


  }
  # преобразовать в даты
  df[, "D1"] <- as.Date(as.POSIXct(df[, "D1"])) + 1
  df[, "DStart"] <- as.Date(as.POSIXct(df[, "DStart"])) + 1
  df[, "DFinish"] <- as.Date(as.POSIXct(df[, "DFinish"])) + 1
  return(df)
  # Тип выплаты: RD - погашение; WT - купонная выплата, PP - частичное
  # погашение
}

# Аналитические показатели рынка ГКО-ОФЗ-ОБР (как XMLDocument)
GKOOFZ_AnalitXML <- function(DateFrom, DateTo) {
  h <- basicTextGatherer()  #фунция для обработки http-запросов
  url <- "http://cbr.ru/secinfo/secinfo.asmx"
  # сформировать тело SOAP запроса
  body <- paste("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n      <soap:Body>\n      <GKOOFZ_AnalitXML xmlns=\"http://web.cbr.ru/\">\n      <DateFrom>",
    DateFrom, "</DateFrom>\n      <DateTo>", DateTo, "</DateTo>\n      </GKOOFZ_AnalitXML>\n      </soap:Body>\n      </soap:Envelope>")
  HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*", SOAPAction = "\"http://web.cbr.ru/GKOOFZ_AnalitXML\"",
    `Content-Type` = "text/xml; charset=utf-8")
  curlPerform(url = url, httpheader = HeaderFields, postfields = body,
    writefunction = h$update)
  response <- h$value()  #получение ответа от сервера
  doc <- xmlInternalTreeParse(response)  # создание XML-дерева
  cleanup <- getNodeSet(doc, "//GA")  #выделение ячеек с данными
  vars <- names(cleanup[[1]])  # получить заголовки переменыными
  # сформировать датафрейм исходя из количества переменных и их их длины
  df <- as.data.frame(matrix(nrow = length(cleanup), ncol = length(vars),
    dimnames = list(NULL, vars)))
  # пройтись по всем переменным в цикле
  for (i in 1:length(vars)) {
    cur <- vars[i]
    cc <- getNodeSet(doc, paste("//", cur, sep = ""))  #получить список значений XML ячейки
    df[, i] <- sapply(cc, xmlValue)  # преобразовать список в вектор
  }
  df[, "RP_DATE"] <- as.Date(as.POSIXct(df[, "RP_DATE"]))
  return(df)

}


# ---------------------------------------------------------------------------------------------
# Факторы формирования ликвидности банковского сектора (как
# XMLDocument) http://www.cbr.ru/scripts/SecInfo_XSD/Flikvid.xsd

FlikvidXML <- function(OnDate, ToDate) {
  doc <- SecFunction2("FlikvidXML", OnDate, ToDate)
  df <- Doc2Df(doc, "FL")
  df[, "D0"] <- as.Date(as.POSIXct(df[, "D0"]))
  df[, 2:6] <- apply(df[, 2:6], 2, as.numeric)
  # df$D0 <- df$D0+1 #прибавить один день, чтобы данные соответствовали
  # ЦБ РФ
  names(df) <- c("date", "fx.interv", "cash", "govt", "reserve.req",
    "liq", "ostatki")
  # Дата проведения Интервенции Банка России на внутреннем валютном рынке
  # Изменение наличных денег в обращении (вне Банка России) Изменение
  # остатков средств на счетах расширенного правительства в Банке России
  # и прочие операции Регулирование Банком России обязательных резервов
  # кредитных организаций Нетто-объем операций Банка России по
  # предоставлению и абсорбированию ликвидности (без учета операций на
  # внутреннем валютном рынке) Остатки средств на корреспонденских счетах
  # КО в Банке России на конец дня
  return(df)

}


# Получение динамики ежедневных курсов валюты (как XMLDocument)
GetCursDynamicXML <- function(FromDate, ToDate, ValutaCode) {
  if (!FromDate < ToDate) {
    stop("Dates sequence error")
  }
  h <- basicTextGatherer()
  url <- "http://www.cbr.ru/DailyInfoWebServ/DailyInfo.asmx"
  body <- paste("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n  <soap:Body>\n    <GetCursDynamicXML xmlns=\"http://web.cbr.ru/\">\n      <FromDate>",
    FromDate, "</FromDate>\n      <ToDate>", ToDate, "</ToDate>\n      <ValutaCode>",
    ValutaCode, "</ValutaCode>\n    </GetCursDynamicXML>\n  </soap:Body>\n</soap:Envelope>",
    sep = "")
  HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*", SOAPAction = "\"http://web.cbr.ru/GetCursDynamicXML\"",
    `Content-Type` = "text/xml; charset=utf-8")
  curlPerform(url = url, httpheader = HeaderFields, postfields = body,
    writefunction = h$update)
  response <- h$value()
  doc <- xmlInternalTreeParse(response)
  df <- Doc2Df(doc, "ValuteCursDynamic")
  cleanup <- getNodeSet(doc, "//ValuteCursDynamic")

  df[, "CursDate"] <- as.Date(as.POSIXct(df[, "CursDate"]))
  df <- df[, c(1, 4)]
  df <- xts(as.numeric(as.character(df[, 2])), order.by = df[, 1])
  return(df)
}


# Функция: Срочная структура процентных ставок (Изотермный ряд
# бескупонной доходности) (как DataSet) Схема:
# http://www.cbr.ru/scripts/SecInfo_XSD/Isoterm.xsd Isoterm <-
# function(OnDate, ToDate, ValutaCode, I_Day){ h <- basicTextGatherer()
# url <- 'http://www.cbr.ru/secinfo/secinfo.asmx' body <-paste('<?xml
# version='1.0' encoding='utf-8'?> <soap:Envelope
# xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'
# xmlns:xsd='http://www.w3.org/2001/XMLSchema'
# xmlns:soap='http://schemas.xmlsoap.org/soap/envelope/'> <soap:Body>
# <Isoterm xmlns='http://web.cbr.ru/'> <OnDate>', OnDate,'</OnDate>
# <ToDate>', ToDate,'</ToDate> <I_Day>', I_Day,'</I_Day> </Isoterm>
# </soap:Body> </soap:Envelope>') HeaderFields=c(Accept='text/xml',
# Accept='multipart/*', SOAPAction=''http://web.cbr.ru/Isoterm'',
# 'Content-Type' = 'text/xml; charset=utf-8') curlPerform(url = url,
# httpheader = HeaderFields, postfields = body, writefunction =
# h$update) response <- h$value() doc <- xmlInternalTreeParse(response)
# cleanup <- getNodeSet(doc, '//It') vars <- names(cleanup[[1]]) df <-
# as.data.frame(matrix(nrow =length(cleanup), ncol = length(vars),
# dimnames = list(NULL, vars))) for (i in 1:length(vars)){ cur <-
# vars[i] cc <- getNodeSet(doc, paste('//',cur, sep ='')) df[,i] <-
# sapply(cc, xmlValue) } df[, 'day']<- as.Date(as.POSIXct(df[,
# 'day']))+1 df[, 'val']<- as.numeric(df[, 'val']) return(df) }

MainInfoXML <- function() {
  name <- "MainInfoXML"
  h <- basicTextGatherer()  #фунция для обработки http-запросов
  url <- "http://www.cbr.ru/DailyInfoWebServ/DailyInfo.asmx"
  # сформировать тело SOAP запроса
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <",
    name, " xmlns=\"http://web.cbr.ru/\">\n                </", name,
    ">\n                </soap:Body>\n                </soap:Envelope>")
  HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*", SOAPAction = paste("\"http://web.cbr.ru/",
    name, "\"", sep = ""), `Content-Type` = "text/xml; charset=utf-8")
  curlPerform(url = url, httpheader = HeaderFields, postfields = body,
    writefunction = h$update)
  response <- h$value()  #получение ответа от сервера
  doc <- xmlInternalTreeParse(response)  # создание XML-дерева

}


# Сальдо операций ЦБ РФ по предоставлению/абсорбированию ликвидности
# (XMLDocument)
SaldoXML <- function(fromDate, ToDate) {
  doc <- DailyFunction("SaldoXML", fromDate, ToDate)
  df <- Doc2Df(doc, "So")
  df[, "Dt"] <- as.Date(as.POSIXct(df[, "Dt"])) + 1
  names(df) <- c("date", "Saldo")
  df[, "Saldo"] <- as.numeric(as.character(df[, "Saldo"]))
  df <- xts((df[, -1]), order.by = df[, 1])
  return(df)
}

# Ставка Ruonia (XMLDocument)
RuoniaXML <- function(fromDate, ToDate) {
  doc <- DailyFunction("RuoniaXML", fromDate, ToDate)
  df <- Doc2Df(doc, "ro")
  df[, "D0"] <- as.Date(as.POSIXct(df[, "D0"])) + 1
  df[, "ruo"] <- as.numeric(as.character(df[, "ruo"]))
  df[, "vol"] <- as.numeric(as.character(df[, "vol"]))
  df <- xts((df[, -1]), order.by = df[, 1])
  return(df)
}

# Требования Банка России к кредитным организациям (как xmlDocument)
DVXML <- function(fromDate, ToDate) {
  doc <- DailyFunction("DVXML", fromDate, ToDate)
  df <- Doc2Df(doc, "DV")
  df[, "Date"] <- as.Date(as.POSIXct(df[, "Date"])) + 1
  df <- df[, !(names(df) %in% "VIDate")]
  df <- df[, !(names(df) %in% "Vol_Gold")]
  df[, 2:ncol(df)] <- apply(df[, 2:ncol(df)], 2, as.numeric)
  df <- xts((df[, -1]), order.by = df[, 1])
  return(df)
}

# Валютный своп buy/sell overnight (XMLDocument)
SwapDynamicXML <- function(fromDate, ToDate) {
  doc <- DailyFunction("SwapDynamicXML", fromDate, ToDate)
  df <- Doc2Df(doc, "Swap")
  df[, "DateBuy"] <- as.Date(as.POSIXct(df[, "DateBuy"])) + 1
  df[, "DateSell"] <- as.Date(as.POSIXct(df[, "DateSell"])) + 1
  return(df)
}

# База данных по размещению бюджетных средств на депозиты коммерческих
# банков (как xmlDocument
BauctionXML <- function(fromDate, ToDate) {
  doc <- DailyFunction("BauctionXML", fromDate, ToDate)
  df <- Doc2Df(doc, "BA")
  df[, "date"] <- as.Date(as.POSIXct(df[, "date"])) + 1
  df[, 2:4] <- apply(df[, 2:4], 2, as.numeric)
  return(df)
}

# Динамики сведений об остатках средств на корреспондентских счетах
# кредитных организаций (XMLDocument).
OstatDynamicXML <- function(fromDate, ToDate) {
  doc <- DailyFunction("OstatDynamicXML", fromDate, ToDate)
  df <- Doc2Df(doc, "Ostat")
  df[, "DateOst"] <- as.Date(as.POSIXct(df[, "DateOst"])) + 1
  df[, 2:4] <- apply(df[, 2:4], 2, as.numeric)
  df <- xts((df[, -1]), order.by = df[, 1])
  return(df)
}

# Ставка ROISfix (XMLDocument) Задолженность кредитных организаций
# перед Банком России по операциям прямого РЕПО (как xmlDocument)

Repo_debtXML <- function(fromDate, ToDate) {
  doc <- DailyFunction("Repo_debtXML", fromDate, ToDate)
  df <- Doc2Df(doc, "RD")
  df[, "Date"] <- as.Date(as.POSIXct(df[, "Date"])) + 1
  df[, -1] <- apply(df[, -1], 2, as.numeric)
  df <- xts((df[, -1]), order.by = df[, 1])
  return(df)
}

# Операции Банка России на рынке государственных ценных бумаг по
# поручению Министерства финансов Российской Федерации (XMLDocument) не
# работает
XVolXML <- function(fromDate, ToDate) {
  doc <- DailyFunction("XVolXML", fromDate, ToDate)
  df <- Doc2Df(doc, "RD")
  df[, "Date"] <- as.Date(as.POSIXct(df[, "Date"])) + 1
  df[, 2] <- as.numeric(df[, 2])
  df <- xts((df[, -1]), order.by = df[, 1])
  return(df)
}

# Международные резервы Российской Федерации, еженедельные значения
# (XMLDocument)
mrrf7DXML <- function(fromDate, ToDate) {
  doc <- DailyFunction("mrrf7DXML", fromDate, ToDate)
  df <- Doc2Df(doc, "mr")
  df[, "D0"] <- as.Date(as.POSIXct(df[, "D0"])) + 1
  df[, 2] <- as.numeric(as.character(df[, 2]))
  df <- xts((df[, -1]), order.by = df[, 1])
  return(df)
}

# Фиксированная процентная ставка по ломбардным кредитам Банка России
# сроком на один календарный день (как XMLDocument)
FixedStav1DayXML <- function(OnDate, ToDate) {
  doc <- SecFunction2("FixedStav1DayXML", OnDate, ToDate)
  df <- Doc2Df(doc, "fs")
  df[, "D0"] <- as.Date(as.POSIXct(df[, "D0"])) + 1
  df[, 2] <- as.numeric(as.character(df[, 2]))
  df <- xts((df[, -1]), order.by = df[, 1])
  return(df)
}

# Фиксированные процентные ставки по кредитам, обеспеченным нерыночными
# активами или поручительствами (как XMLDocument)
NonMarketCreditXML <- function(OnDate, ToDate) {
  doc <- SecFunction2("NonMarketCreditXML", OnDate, ToDate)
  df <- Doc2Df(doc, "nc")
  df[, "D0"] <- as.Date(as.POSIXct(df[, "D0"])) + 1
  df[, 2:4] <- apply(df[, 2:4], 2, as.numeric)
  df <- xts((df[, -1]), order.by = df[, 1])
  return(df)
}

# Информация о предоставлении кредитов без обеспечения кредитным
# организациям (как XMLDocument)
UnSecLoansXML <- function(DateFrom, c) {
  doc <- SecFunction("UnSecLoansXML", DateFrom, DateTo)
  df <- Doc2Df(doc, "UL")
  df[, "PDate"] <- as.Date(as.POSIXct(df[, "PDate"])) + 1
  df[, "DEPOSIT_DATE"] <- as.Date(as.POSIXct(df[, "DEPOSIT_DATE"])) +
    1
  df[, "MATURITY_DATE"] <- as.Date(as.POSIXct(df[, "MATURITY_DATE"])) +
    1
  df[, 4:length(names(df))] <- apply(df[, 4:length(names(df))], 2, as.numeric)
  df <- xts((df[, -1]), order.by = df[, 1])
  return(df)
}

# Задолженность по обеспеченным кредитам Банка России (как XMLDocument)
SecLoansDebtXML <- function(DateFrom, DateTo) {
  doc <- SecFunction2("SecLoansDebtXML", DateFrom, DateTo)
  df <- Doc2Df(doc, "SL")
  df[, "D0"] <- as.Date(as.POSIXct(df[, "D0"])) + 1
  df[, 2:length(names(df))] <- apply(df[, 2:length(names(df))], 2, as.numeric)
  df <- xts((df[, -1]), order.by = df[, 1])
  return(df)
  # =======
  library(XML)
  library(RCurl)
  library(SSOAP)
  library(xts)
  library(ggplot2)
  library(gridExtra)
  library(scales)
  library(reshape2)
  library(RColorBrewer)
  library(quantmod)
  library(PerformanceAnalytics)
  library(dplyr)

  # install.packages('SSOAP', repos = 'http://www.omegahat.org/R',
  # dependencies = TRUE, type = 'source') Генерическая функция обращения
  # к разделу Банка России для получения информации по рынку ценных бумаг
  # http://cbr.ru/scripts/Root.asp?Prtid=SEC
  SecFunction <- function(name, DateFrom, DateTo) {
    h <- basicTextGatherer()  #фунция для обработки http-запросов
    url <- "http://cbr.ru/secinfo/secinfo.asmx"
    # сформировать тело SOAP запроса
    body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <",
      name, " xmlns=\"http://web.cbr.ru/\">\n                <DateFrom>",
      DateFrom, "</DateFrom>\n                <DateTo>", DateTo,
      "</DateTo>\n                </", name, ">\n                </soap:Body>\n                </soap:Envelope>")
    HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*",
      SOAPAction = paste("\"http://web.cbr.ru/", name, "\"", sep = ""),
      `Content-Type` = "text/xml; charset=utf-8")
    curlPerform(url = url, httpheader = HeaderFields, postfields = body,
      writefunction = h$update)
    response <- h$value()  #получение ответа от сервера
    doc <- xmlInternalTreeParse(response)  # создание XML-дерева
    return(doc)

  }

  SecFunction2 <- function(name, OnDate, ToDate) {
    h <- basicTextGatherer()  #фунция для обработки http-запросов
    url <- "http://cbr.ru/secinfo/secinfo.asmx"
    # сформировать тело SOAP запроса
    body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <",
      name, " xmlns=\"http://web.cbr.ru/\">\n                <OnDate>",
      OnDate, "</OnDate>\n                <ToDate>", ToDate, "</ToDate>\n                </",
      name, ">\n                </soap:Body>\n                </soap:Envelope>")
    HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*",
      SOAPAction = paste("\"http://web.cbr.ru/", name, "\"", sep = ""),
      `Content-Type` = "text/xml; charset=utf-8")
    curlPerform(url = url, httpheader = HeaderFields, postfields = body,
      writefunction = h$update)
    response <- h$value()  #получение ответа от сервера
    doc <- xmlInternalTreeParse(response)  # создание XML-дерева
    return(doc)

  }

  SecFunction3 <- function(name, dt) {
    h <- basicTextGatherer()  #фунция для обработки http-запросов
    url <- "http://cbr.ru/secinfo/secinfo.asmx"
    # сформировать тело SOAP запроса
    body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <",
      name, " xmlns=\"http://web.cbr.ru/\">\n                <dt>",
      dt, "</dt>\n                \n                </", name, ">\n                </soap:Body>\n                </soap:Envelope>")
    HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*",
      SOAPAction = paste("\"http://web.cbr.ru/", name, "\"", sep = ""),
      `Content-Type` = "text/xml; charset=utf-8")
    curlPerform(url = url, httpheader = HeaderFields, postfields = body,
      writefunction = h$update)
    response <- h$value()  #получение ответа от сервера
    doc <- xmlInternalTreeParse(response)  # создание XML-дерева
    return(doc)

  }
  # Генерическая функция обращения к разделу Банка России 'Веб - сервис
  # для получения ежедневных данных'г
  # http://cbr.ru/scripts/Root.asp?Prtid=DWS

  DailyFunction <- function(name, fromDate, ToDate) {
    h <- basicTextGatherer()  #фунция для обработки http-запросов
    url <- "http://www.cbr.ru/DailyInfoWebServ/DailyInfo.asmx"
    # сформировать тело SOAP запроса
    body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <",
      name, " xmlns=\"http://web.cbr.ru/\">\n                <fromDate>",
      fromDate, "</fromDate>\n                <ToDate>", ToDate,
      "</ToDate>\n                </", name, ">\n                </soap:Body>\n                </soap:Envelope>")
    HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*",
      SOAPAction = paste("\"http://web.cbr.ru/", name, "\"", sep = ""),
      `Content-Type` = "text/xml; charset=utf-8")
    curlPerform(url = url, httpheader = HeaderFields, postfields = body,
      writefunction = h$update)
    response <- h$value()  #получение ответа от сервера
    doc <- xmlInternalTreeParse(response)  # создание XML-дерева
    return(doc)

  }



  # Функция преобразования в результатов генерических функций Daily и Sec
  # в итоговый датафрейм аргументы: doc - XML-дерево
  # (xmlInternalTreeParse) : 'headtag - тег первого уровня в ответе SOAP
  Doc2Df <- function(doc, headtag) {
    cleanup <- getNodeSet(doc, paste0("//", headtag))  # создаем список, разбивая по headtag
    vars <- names(cleanup[[length(cleanup)]])  # имена переменных. Использует последняя точка для получения названий
    dd <- lapply(cleanup, function(x) {
      # применить преобразования к каждому элементу списка cleanup
      a <- getNodeSet(x, vars)
      names <- sapply(a, xmlName)  # выделить названия переменных - атрибуты ячеек xml
      values <- sapply(a, xmlValue)  # выделить значения переменных
      names(values) <- names
      nots <- vars[!(vars %in% names(values))]  # если какие-то ячейки отсутствуют в данной ноде
      values[nots] <- NA  # то ставим NA
      return(values)
    })
    df <- as.data.frame(do.call(rbind, dd))
    return(df)


  }

  # Условия проведения Банком России операций по предоставлению кредитным
  # организациям обеспеченных кредитов Банка России по фиксированным
  # процентным ставкам (кроме кредитов овернайт)
  creditco <- function(dt) {
    doc <- SecFunction3("creditco", dt)
    Doc2Df(doc, "credit")
  }

  # Параметры курсовой политики Банка России (как XMLDocument)
  Sp_fxpm_XML <- function(fromDate, DateTo) {
    doc <- SecFunction("Sp_fxpm_XML", fromDate, DateTo)
    df <- Doc2Df(doc, "Data/Corridor")
    df1 <- Doc2Df(doc, "Data/SmoothingInterventions")
    df2 <- Doc2Df(doc, "Data/InternalRangesWidth")
    df3 <- as.data.frame(sapply(getNodeSet(doc, paste0("//", "D0")),
      xmlValue))
    names(df3) <- "D0"
    df4 <- as.data.frame(sapply(getNodeSet(doc, paste0("//", "CumulativeVolume")),
      xmlValue))
    names(df4) <- "CumulativeVolume"
    df5 <- as.data.frame(sapply(getNodeSet(doc, paste0("//", "TreasuryOrderVolulme")),
      xmlValue))
    names(df5) <- "TreasuryOrderVolulme"
    df <- cbind(df, df1, df2, df3, df4, df5)

    df[, "D0"] <- as.Date(as.POSIXct(df[, "D0"])) + 1
    dt <- as.Date(as.POSIXct(df[, "D0"])) + 1
    df$D0 <- NULL
    df <- apply(df, 2, as.numeric)
    df <- xts(df, order.by = dt)
    return(df)


  }






  # Ставки межбанковского кредитного рынка (как xmlDocument)
  MKRXML <- function(fromDate, ToDate) {
    doc <- DailyFunction("MKRXML", fromDate, ToDate)
    df <- Doc2Df(doc, "MKR")
    df[, "CDate"] <- as.Date(as.POSIXct(df[, "CDate"])) + 1
    df[, "p1"] <- as.factor(df[, "p1"])
    # p1 :тип 1-MIBID, 2-MIBOR, 3-MIACR, 4-MIACR-IG, 5-MIACR-B,
    # 6-MIACR(оборот), 7-MIACR-IG(оборот), 8-MIACR-B(оборот)
    df[, 3:length(names(df))] <- apply(df[, 3:length(names(df))], 2,
      as.numeric)  #преобразовать остальные столбцы в числа
    return(df)
  }

  # Функция DepoDynamicXML
  # http://cbr.ru/DailyInfoWebServ/DailyInfo.asmx?op=DepoDynamicXML
  # Динамики ставок привлечения средств по депозитным операциям (как
  # xmlDocument

  DepoDynamicXML <- function(fromDate, ToDate) {
    doc <- DailyFunction("DepoDynamicXML", fromDate, ToDate)
    df <- Doc2Df(doc, "Depo")
    df[, "DateDepo"] <- as.Date(as.POSIXct(df[, "DateDepo"])) + 1
    df[, 2:length(names(df))] <- apply(df[, 2:length(names(df))], 2,
      as.numeric)  #преобразовать остальные столбцы в числа
    df <- xts((df[, -1]), order.by = df[, 1])
    return(df)

  }

  # Сессия прямого РЕПО по фиксированной ставке (как XMLDocument)
  RepoSessionXML <- function(DateFrom, DateTo) {
    doc <- SecFunction("RepoSessionXML", DateFrom, DateTo)
    df <- Doc2Df(doc, "RS")
    df[, "D0"] <- as.Date(as.POSIXct(df[, "D0"])) + 1
    df[, 2:length(names(df))] <- apply(df[, 2:length(names(df))], 2,
      as.numeric)
    df <- xts((df[, -1]), order.by = df[, 1])
    return(df)

  }

  # Параметры аукционов прямого РЕПО (как XMLDocument)
  DirRepoAuctionParamXML <- function(OnDate, ToDate) {
    doc <- SecFunction2("DirRepoAuctionParamXML", OnDate, ToDate)
    df <- Doc2Df(doc, "DR")
    df[, "D0"] <- as.Date(as.POSIXct(df[, "D0"])) + 1
    df <- xts((df[, -1]), order.by = df[, 1])
    return(df)

  }

  # MosPrime Rate (как XMLDocument)
  MosPrimeXML <- function(DateFrom, DateTo) {
    doc <- SecFunction("MosPrimeXML", DateFrom, DateTo)
    df <- Doc2Df(doc, "MP")
    df[, "MP_Date"] <- as.Date(as.POSIXct(df[, "MP_Date"])) + 1
    df[, 2:length(names(df))] <- apply(df[, 2:length(names(df))], 2,
      as.numeric)
    df <- xts((df[, -1]), order.by = df[, 1])
    return(df)
  }

  # Итоги аукциона прямого РЕПО (как XMLDocument) <xs:element name='Dt'
  # msdata:Caption='Дата' type='xs:dateTime'/> <xs:element
  # name='avg_deal' msdata:Caption='Общий объем заключенных сделок, млн.'
  # type='xs:decimal' minOccurs='0'/> <xs:element name='avg_yield'
  # msdata:Caption='Средневзвешенная ставка, % годовых' type='xs:decimal'
  # minOccurs='0'/> <xs:element name='snd_date' msdata:Caption='Дата
  # исполнения второй части РЕПО' type='xs:dateTime' minOccurs='0'/>
  # <xs:element name='day_repo' msdata:Caption='Срок РЕПО, дни'
  # type='xs:decimal' minOccurs='0'/> <xs:element name='mTime'
  # msdata:Caption='время аукциона' type='xs:string'/> <xs:element
  # name='PCode' msdata:Caption='Код расчетов по сделкам'
  # type='xs:string' minOccurs='0'/> <xs:element name='bid'
  # msdata:Caption='Объем спроса, млн. руб.' type='xs:decimal'
  # minOccurs='0'/> <xs:element name='max_state_intrate'
  # msdata:Caption='Максимальная заявленная ставка, % годовых'
  # type='xs:decimal' minOccurs='0'/> <xs:element
  # name='min_state_intrate' msdata:Caption='Минимальная заявленная
  # ставка, % годовых' type='xs:decimal' minOccurs='0'/> <xs:element
  # name='cut_off_rate' msdata:Caption='Ставка отсечения, % годовых'
  # type='xs:decimal' minOccurs='0'/> <xs:element name='avg_deal_lim'
  # msdata:Caption='Объем заключенных сделок в рамках лимита, млн. руб.'
  # type='xs:decimal' minOccurs='0'/> <xs:element name='avg_yield_lim'
  # msdata:Caption='Средневзвешенная ставка по заявкам, удовлетворенным в
  # рамках лимита, % годовых' type='xs:decimal' minOccurs='0'/>
  # <xs:element name='first_date' msdata:Caption='Дата исполнения первой
  # части сделки РЕПО' type='xs:dateTime' minOccurs='0'/> </xs:sequence>

  REPOXML <- function(DateFrom, DateTo) {
    doc <- SecFunction("REPOXML", DateFrom, DateTo)
    df <- Doc2Df(doc, "RP")
    df[, "Dt"] <- as.Date(as.POSIXct(df[, "Dt"])) + 1
    df[, "snd_date"] <- as.Date(as.POSIXct(df[, "snd_date"])) + 1
    df[, "first_date"] <- as.Date(as.POSIXct(df[, "first_date"])) +
      1
    num <- c("avg_deal", "avg_yield", "bid", "max_state_intrate", "min_state_intrate",
      "cut_off_rate", "avg_deal_lim", "avg_yield_lim")
    df[, num] <- apply(df[, num], 2, as.numeric)
    return(df)

  }

  # Информация по аукционам ГКО-ОФЗ (как XMLDocument) !!! только по 1
  # января 2013 года

  AuctionsXML <- function(DateFrom, DateTo) {
    h <- basicTextGatherer()  #фунция для обработки http-запросов
    url <- "http://cbr.ru/secinfo/secinfo.asmx"
    # сформировать тело SOAP запроса
    body <- paste("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <AuctionsXML xmlns=\"http://web.cbr.ru/\">\n                <DateFrom>",
      DateFrom, "</DateFrom>\n                <DateTo>", DateTo,
      "</DateTo>\n                </AuctionsXML>\n                </soap:Body>\n                </soap:Envelope>")


    HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*",
      SOAPAction = "\"http://web.cbr.ru/AuctionsXML\"", `Content-Type` = "text/xml; charset=utf-8")
    curlPerform(url = url, httpheader = HeaderFields, postfields = body,
      writefunction = h$update)
    response <- h$value()  #получение ответа от сервера
    doc <- xmlInternalTreeParse(response)  # создание XML-дерева
    cleanup <- getNodeSet(doc, "//AU")  #выделение ячеек с данными
    vars <- names(cleanup[[1]])
    df <- as.data.frame(matrix(nrow = length(cleanup), ncol = length(vars),
      dimnames = list(NULL, vars)))
    a <- cleanup[[1]]
    for (j in 1:length(cleanup)) {
      # по всем строкам
      a <- cleanup[[j]]  # выделить отдельную ячейку и пройтись по ней
      for (i in 1:length(vars)) {
        cur <- vars[i]
        cc <- getNodeSet(a, cur)  #получить список значений XML ячейки
        drow <- (sapply(cc, xmlValue))  # преобразовать список в вектор
        if (length(drow) > 0)
          df[j, i] <- drow else df[j, i] <- NA  #если нет каких-то значений, то поставить NA
      }
      # print(j)
    }
    # преобразовать в даты
    df[, "d0"] <- as.Date(as.POSIXct(df[, "d0"])) + 1
    df[, "BO_FINISH"] <- as.Date(as.POSIXct(df[, "BO_FINISH"])) + 1
    df[, 7:19] <- apply(df[, 7:19], 2, as.numeric)
    return(df)

  }

  # База данных по купонным выплатам и погашениям (как XMLDocument)
  # Cхема: http://www.cbr.ru/scripts/SecInfo_XSD/Coupons.xsd
  CouponsXML <- function(DateFrom, DateTo) {
    doc <- SecFunction("CouponsXML", DateFrom, DateTo)
    df <- Doc2Df(doc, "CP")
    h <- basicTextGatherer()  #фунция для обработки http-запросов
    url <- "http://cbr.ru/secinfo/secinfo.asmx"
    # сформировать тело SOAP запроса
    body <- paste("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <CouponsXML xmlns=\"http://web.cbr.ru/\">\n                <DateFrom>",
      DateFrom, "</DateFrom>\n                <DateTo>", DateTo,
      "</DateTo>\n                </CouponsXML>\n                </soap:Body>\n                </soap:Envelope>")

    HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*",
      SOAPAction = "\"http://web.cbr.ru/CouponsXML\"", `Content-Type` = "text/xml; charset=utf-8")
    curlPerform(url = url, httpheader = HeaderFields, postfields = body,
      writefunction = h$update)
    response <- h$value()  #получение ответа от сервера
    doc <- xmlInternalTreeParse(response)  # создание XML-дерева
    cleanup <- getNodeSet(doc, "//CP")  #выделение ячеек с данными
    vars <- names(cleanup[[1]])  # получить заголовки переменыными
    # сформировать датафрейм исходя из количества переменных и их их длины
    df <- as.data.frame(matrix(nrow = length(cleanup), ncol = length(vars),
      dimnames = list(NULL, vars)))
    # пройтись по всем переменным в цикле (вложенный цикл необходим так в
    # некоторых элементах есть не все ячейки)
    a <- cleanup[[1]]
    for (j in 1:length(cleanup)) {
      # по всем строкам
      a <- cleanup[[j]]  # выделить отдельную ячейку и пройтись по ней
      for (i in 1:length(vars)) {
        cur <- vars[i]
        cc <- getNodeSet(a, cur)  #получить список значений XML ячейки
        drow <- (sapply(cc, xmlValue))  # преобразовать список в вектор
        if (length(drow) > 0)
          df[j, i] <- drow else df[j, i] <- NA  #если нет каких-то значений, то поставить NA
      }
      # print(j)


    }
    # преобразовать в даты
    df[, "D1"] <- as.Date(as.POSIXct(df[, "D1"])) + 1
    df[, "DStart"] <- as.Date(as.POSIXct(df[, "DStart"])) + 1
    df[, "DFinish"] <- as.Date(as.POSIXct(df[, "DFinish"])) + 1
    return(df)
    # Тип выплаты: RD - погашение; WT - купонная выплата, PP - частичное
    # погашение
  }

  # Аналитические показатели рынка ГКО-ОФЗ-ОБР (как XMLDocument)
  GKOOFZ_AnalitXML <- function(DateFrom, DateTo) {
    h <- basicTextGatherer()  #фунция для обработки http-запросов
    url <- "http://cbr.ru/secinfo/secinfo.asmx"
    # сформировать тело SOAP запроса
    body <- paste("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n      <soap:Body>\n      <GKOOFZ_AnalitXML xmlns=\"http://web.cbr.ru/\">\n      <DateFrom>",
      DateFrom, "</DateFrom>\n      <DateTo>", DateTo, "</DateTo>\n      </GKOOFZ_AnalitXML>\n      </soap:Body>\n      </soap:Envelope>")
    HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*",
      SOAPAction = "\"http://web.cbr.ru/GKOOFZ_AnalitXML\"", `Content-Type` = "text/xml; charset=utf-8")
    curlPerform(url = url, httpheader = HeaderFields, postfields = body,
      writefunction = h$update)
    response <- h$value()  #получение ответа от сервера
    doc <- xmlInternalTreeParse(response)  # создание XML-дерева
    cleanup <- getNodeSet(doc, "//GA")  #выделение ячеек с данными
    vars <- names(cleanup[[1]])  # получить заголовки переменыными
    # сформировать датафрейм исходя из количества переменных и их их длины
    df <- as.data.frame(matrix(nrow = length(cleanup), ncol = length(vars),
      dimnames = list(NULL, vars)))
    # пройтись по всем переменным в цикле
    for (i in 1:length(vars)) {
      cur <- vars[i]
      cc <- getNodeSet(doc, paste("//", cur, sep = ""))  #получить список значений XML ячейки
      df[, i] <- sapply(cc, xmlValue)  # преобразовать список в вектор
    }
    df[, "RP_DATE"] <- as.Date(as.POSIXct(df[, "RP_DATE"]))
    return(df)

  }

  # ---------------------------------------------------------------------------------------------
  # Факторы формирования ликвидности банковского сектора (как
  # XMLDocument) http://www.cbr.ru/scripts/SecInfo_XSD/Flikvid.xsd

  FlikvidXML <- function(OnDate, ToDate) {
    doc <- SecFunction2("FlikvidXML", OnDate, ToDate)
    df <- Doc2Df(doc, "FL")
    df[, "D0"] <- as.Date(as.POSIXct(df[, "D0"]))
    df[, 2:6] <- apply(df[, 2:6], 2, as.numeric)
    # df$D0 <- df$D0+1 #прибавить один день, чтобы данные соответствовали
    # ЦБ РФ
    names(df) <- c("date", "fx.interv", "cash", "govt", "reserve.req",
      "liq", "ostatki")
    # Дата проведения Интервенции Банка России на внутреннем валютном рынке
    # Изменение наличных денег в обращении (вне Банка России) Изменение
    # остатков средств на счетах расширенного правительства в Банке России
    # и прочие операции Регулирование Банком России обязательных резервов
    # кредитных организаций Нетто-объем операций Банка России по
    # предоставлению и абсорбированию ликвидности (без учета операций на
    # внутреннем валютном рынке) Остатки средств на корреспонденских счетах
    # КО в Банке России на конец дня
    return(df)

  }


  # Получение динамики ежедневных курсов валюты (как XMLDocument)
  GetCursDynamicXML <- function(FromDate, ToDate, ValutaCode) {
    h <- basicTextGatherer()
    url <- "http://www.cbr.ru/DailyInfoWebServ/DailyInfo.asmx"
    body <- paste("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n  <soap:Body>\n    <GetCursDynamicXML xmlns=\"http://web.cbr.ru/\">\n      <FromDate>",
      FromDate, "</FromDate>\n      <ToDate>", ToDate, "</ToDate>\n      <ValutaCode>",
      ValutaCode, "</ValutaCode>\n    </GetCursDynamicXML>\n  </soap:Body>\n</soap:Envelope>",
      sep = "")
    HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*",
      SOAPAction = "\"http://web.cbr.ru/GetCursDynamicXML\"", `Content-Type` = "text/xml; charset=utf-8")
    curlPerform(url = url, httpheader = HeaderFields, postfields = body,
      writefunction = h$update)
    response <- h$value()
    doc <- xmlInternalTreeParse(response)
    df <- Doc2Df(doc, "ValuteCursDynamic")
    cleanup <- getNodeSet(doc, "//ValuteCursDynamic")

    df[, "CursDate"] <- as.Date(as.POSIXct(df[, "CursDate"]))
    df <- df[, c(1, 4)]
    df <- xts(as.numeric(as.character(df[, 2])), order.by = df[, 1])
    return(df)
  }


  # Функция: Срочная структура процентных ставок (Изотермный ряд
  # бескупонной доходности) (как DataSet) Схема:
  # http://www.cbr.ru/scripts/SecInfo_XSD/Isoterm.xsd
  Isoterm <- function(OnDate, ToDate, ValutaCode, I_Day) {
    h <- basicTextGatherer()
    url <- "http://www.cbr.ru/secinfo/secinfo.asmx"
    body <- paste("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n  <soap:Body>\n    <Isoterm xmlns=\"http://web.cbr.ru/\">\n      <OnDate>",
      OnDate, "</OnDate>\n      <ToDate>", ToDate, "</ToDate>\n      <I_Day>",
      I_Day, "</I_Day>\n    </Isoterm>\n  </soap:Body>\n</soap:Envelope>")
    HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*",
      SOAPAction = "\"http://web.cbr.ru/Isoterm\"", `Content-Type` = "text/xml; charset=utf-8")
    curlPerform(url = url, httpheader = HeaderFields, postfields = body,
      writefunction = h$update)
    response <- h$value()
    doc <- xmlInternalTreeParse(response)
    cleanup <- getNodeSet(doc, "//It")
    vars <- names(cleanup[[1]])
    df <- as.data.frame(matrix(nrow = length(cleanup), ncol = length(vars),
      dimnames = list(NULL, vars)))
    for (i in 1:length(vars)) {
      cur <- vars[i]
      cc <- getNodeSet(doc, paste("//", cur, sep = ""))
      df[, i] <- sapply(cc, xmlValue)
    }
    df[, "day"] <- as.Date(as.POSIXct(df[, "day"])) + 1
    df[, "val"] <- as.numeric(df[, "val"])
    return(df)
  }

  MainInfoXML <- function() {
    name <- "MainInfoXML"
    h <- basicTextGatherer()  #фунция для обработки http-запросов
    url <- "http://www.cbr.ru/DailyInfoWebServ/DailyInfo.asmx"
    # сформировать тело SOAP запроса
    body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <",
      name, " xmlns=\"http://web.cbr.ru/\">\n                </",
      name, ">\n                </soap:Body>\n                </soap:Envelope>")
    HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*",
      SOAPAction = paste("\"http://web.cbr.ru/", name, "\"", sep = ""),
      `Content-Type` = "text/xml; charset=utf-8")
    curlPerform(url = url, httpheader = HeaderFields, postfields = body,
      writefunction = h$update)
    response <- h$value()  #получение ответа от сервера
    doc <- xmlInternalTreeParse(response)  # создание XML-дерева

  }


  # Сальдо операций ЦБ РФ по предоставлению/абсорбированию ликвидности
  # (XMLDocument)
  SaldoXML <- function(fromDate, ToDate) {
    doc <- DailyFunction("SaldoXML", fromDate, ToDate)
    df <- Doc2Df(doc, "So")
    df[, "Dt"] <- as.Date(as.POSIXct(df[, "Dt"])) + 1
    names(df) <- c("date", "Saldo")
    df[, "Saldo"] <- as.numeric(as.character(df[, "Saldo"]))
    df <- xts((df[, -1]), order.by = df[, 1])
    return(df)
  }

  # Ставка Ruonia (XMLDocument)
  RuoniaXML <- function(fromDate, ToDate) {
    doc <- DailyFunction("RuoniaXML", fromDate, ToDate)
    df <- Doc2Df(doc, "ro")
    df[, "D0"] <- as.Date(as.POSIXct(df[, "D0"])) + 1
    df[, "ruo"] <- as.numeric(as.character(df[, "ruo"]))
    df[, "vol"] <- as.numeric(as.character(df[, "vol"]))
    df <- xts((df[, -1]), order.by = df[, 1])
    return(df)
  }

  # Требования Банка России к кредитным организациям (как xmlDocument)
  DVXML <- function(fromDate, ToDate) {
    doc <- DailyFunction("DVXML", fromDate, ToDate)
    df <- Doc2Df(doc, "DV")
    df[, "Date"] <- as.Date(as.POSIXct(df[, "Date"])) + 1
    df <- df[, !(names(df) %in% "VIDate")]
    df <- df[, !(names(df) %in% "Vol_Gold")]
    df[, 2:ncol(df)] <- apply(df[, 2:ncol(df)], 2, as.numeric)
    df <- xts((df[, -1]), order.by = df[, 1])
    return(df)
  }

  # Валютный своп buy/sell overnight (XMLDocument)
  SwapDynamicXML <- function(fromDate, ToDate) {
    doc <- DailyFunction("SwapDynamicXML", fromDate, ToDate)
    df <- Doc2Df(doc, "Swap")
    df[, "DateBuy"] <- as.Date(as.POSIXct(df[, "DateBuy"])) + 1
    df[, "DateSell"] <- as.Date(as.POSIXct(df[, "DateSell"])) + 1
    return(df)
  }

  # База данных по размещению бюджетных средств на депозиты коммерческих
  # банков (как xmlDocument
  BauctionXML <- function(fromDate, ToDate) {
    doc <- DailyFunction("BauctionXML", fromDate, ToDate)
    df <- Doc2Df(doc, "BA")
    df[, "date"] <- as.Date(as.POSIXct(df[, "date"])) + 1
    df[, 2:4] <- apply(df[, 2:4], 2, as.numeric)
    return(df)
  }

  # Динамики сведений об остатках средств на корреспондентских счетах
  # кредитных организаций (XMLDocument).
  OstatDynamicXML <- function(fromDate, ToDate) {
    doc <- DailyFunction("OstatDynamicXML", fromDate, ToDate)
    df <- Doc2Df(doc, "Ostat")
    df[, "DateOst"] <- as.Date(as.POSIXct(df[, "DateOst"])) + 1
    df[, 2:4] <- apply(df[, 2:4], 2, as.numeric)
    df <- xts((df[, -1]), order.by = df[, 1])
    return(df)
  }

  # Ставка ROISfix (XMLDocument)
  Repo_debtXML <- function(fromDate, ToDate) {
    doc <- DailyFunction("Repo_debtXML", fromDate, ToDate)
    df <- Doc2Df(doc, "RD")
    df[, "Date"] <- as.Date(as.POSIXct(df[, "Date"])) + 1
    df[, 2] <- as.numeric(df[, 2])
    df <- xts((df[, -1]), order.by = df[, 1])
    return(df)
  }

  # Операции Банка России на рынке государственных ценных бумаг по
  # поручению Министерства финансов Российской Федерации (XMLDocument) не
  # работает
  XVolXML <- function(fromDate, ToDate) {
    doc <- DailyFunction("XVolXML", fromDate, ToDate)
    df <- Doc2Df(doc, "RD")
    df[, "Date"] <- as.Date(as.POSIXct(df[, "Date"])) + 1
    df[, 2] <- as.numeric(df[, 2])
    df <- xts((df[, -1]), order.by = df[, 1])
    return(df)
  }

  # Международные резервы Российской Федерации, еженедельные значения
  # (XMLDocument)
  mrrf7DXML <- function(fromDate, ToDate) {
    doc <- DailyFunction("mrrf7DXML", fromDate, ToDate)
    df <- Doc2Df(doc, "mr")
    df[, "D0"] <- as.Date(as.POSIXct(df[, "D0"])) + 1
    df[, 2] <- as.numeric(as.character(df[, 2]))
    df <- xts((df[, -1]), order.by = df[, 1])
    return(df)
  }

  # Фиксированная процентная ставка по ломбардным кредитам Банка России
  # сроком на один календарный день (как XMLDocument)
  FixedStav1DayXML <- function(OnDate, ToDate) {
    doc <- SecFunction2("FixedStav1DayXML", OnDate, ToDate)
    df <- Doc2Df(doc, "fs")
    df[, "D0"] <- as.Date(as.POSIXct(df[, "D0"])) + 1
    df[, 2] <- as.numeric(as.character(df[, 2]))
    df <- xts((df[, -1]), order.by = df[, 1])
    return(df)
  }

  # Фиксированные процентные ставки по кредитам, обеспеченным нерыночными
  # активами или поручительствами (как XMLDocument)
  NonMarketCreditXML <- function(OnDate, ToDate) {
    doc <- SecFunction2("NonMarketCreditXML", OnDate, ToDate)
    df <- Doc2Df(doc, "nc")
    df[, "D0"] <- as.Date(as.POSIXct(df[, "D0"])) + 1
    df[, 2:4] <- apply(df[, 2:4], 2, as.numeric)
    df <- xts((df[, -1]), order.by = df[, 1])
    return(df)
  }

  # Информация о предоставлении кредитов без обеспечения кредитным
  # организациям (как XMLDocument)
  UnSecLoansXML <- function(DateFrom, c) {
    doc <- SecFunction("UnSecLoansXML", DateFrom, DateTo)
    df <- Doc2Df(doc, "UL")
    df[, "PDate"] <- as.Date(as.POSIXct(df[, "PDate"])) + 1
    df[, "DEPOSIT_DATE"] <- as.Date(as.POSIXct(df[, "DEPOSIT_DATE"])) +
      1
    df[, "MATURITY_DATE"] <- as.Date(as.POSIXct(df[, "MATURITY_DATE"])) +
      1
    df[, 4:length(names(df))] <- apply(df[, 4:length(names(df))], 2,
      as.numeric)
    df <- xts((df[, -1]), order.by = df[, 1])
    return(df)
  }

  # Задолженность по обеспеченным кредитам Банка России (как XMLDocument)
  SecLoansDebtXML <- function(DateFrom, DateTo) {
    doc <- SecFunction2("SecLoansDebtXML", DateFrom, DateTo)
    df <- Doc2Df(doc, "SL")
    df[, "D0"] <- as.Date(as.POSIXct(df[, "D0"])) + 1
    df[, 2:length(names(df))] <- apply(df[, 2:length(names(df))], 2,
      as.numeric)
    df <- xts((df[, -1]), order.by = df[, 1])
    return(df)

  }
}
