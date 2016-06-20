#
library(httr)


body <- "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <SaldoXML xmlns=\"http://web.cbr.ru/\">\n<fromDate>2013-01-01</fromDate>\n<ToDate>2014-04-29</ToDate>\n</SaldoXML>\n </soap:Body>\n </soap:Envelope>"


r <- POST("http://cbr.ru/DailyInfoWebServ/DailyInfo.asmx", body = body,
          add_headers('Content-Type' = "text/xml; charset=utf-8",
                      SOAPAction = "http://web.cbr.ru/Bauction",
                      Accept = "text/xml"
                      ), verbose())
a <- GET("http://cbr.ru/DailyInfoWebServ/DailyInfo.asmx")
content(a, "text")

stop_for_status(r)
content(r, "text")


cr <- content(r, "text")
cr
r


library(RCurl)

url <- "http://www.cbr.ru/DailyInfoWebServ/DailyInfo.asmx"

headerFields =
  c(Accept = "text/xml",
    Accept = "multipart/*",
    'Content-Type' = "text/xml; charset=utf-8",
    SOAPAction = "http://web.cbr.ru/Bauction")

curlPerform(url = url,
            httpheader = headerFields,
            postfields = body
)

library(SSOAP)

help(package = "SSOAP")

result <- content(r, type = "text")
content(r)


library("ecb")
help(package = "ecb")

hicp <- get_data("ICP.M.U2.N.000000.4.ANR")
hicp
get_data
