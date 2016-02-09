# Веб - сервис для получения информации справочника по кредитным
# организациям http://www.cbr.ru/scripts/Root.asp?Prtid=WSCO Автор:
# Салихов Марсель (quantviews.blogspot.ru)

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

# общая функция для отправки SOAP запросов
GenericSoap <- function(url, body, name) {
  h <- RCurl::basicTextGatherer()  #фунция для обработки http-запросов

  HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*", SOAPAction = paste("\"http://web.cbr.ru/",
    name, "\"", sep = ""), `Content-Type` = "text/xml; charset=utf-8")
  RCurl::curlPerform(url = url, httpheader = HeaderFields, postfields = body,
    writefunction = h$update)
  response <- h$value()  #получение ответа от сервера
  doc <- XML::xmlInternalTreeParse(response)  # создание XML-дерева
  return(doc)
}

GenericSoap2 <- function(url, body, name) {
  h <- RCurl::basicTextGatherer()  #фунция для обработки http-запросов
  HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*", SOAPAction = paste("\"http://web.cbr.ru/",
    name, "\"", sep = ""), `Content-Type` = "text/xml; charset=utf-8")
  RCurl::curlPerform(url = url, httpheader = HeaderFields, postfields = body,
    writefunction = h$update)
  response <- h$value()  #получение ответа от сервера
  doc <- SSOAP::getReturnNode(response)  # создание XML-дерева
  return(doc)
}

Data101FullXML <- function(DateFrom, DateTo, CredorgNumber, IndCode) {
  url <- "http://www.cbr.ru/CreditInfoWebServ/CreditOrgInfo.asmx"
  # сформировать тело SOAP запроса
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n                <soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <Data101FullXML xmlns=\"http://web.cbr.ru/\">\n                <CredorgNumber>",
    CredorgNumber, "</CredorgNumber>\n                <IndCode>", IndCode,
    "</IndCode>\n                <DateFrom>", DateFrom, "</DateFrom>\n                <DateTo>",
    DateTo, "</DateTo>\n                </Data101FullXML>\n                </soap:Body>\n                </soap:Envelope>")
  name <- "Data101FullXML"
  doc <- GenericSoap(url = url, body = body, name = name)
  return(doc)
}



# Данные КО. формы 101, без оборотов (как XMLDocument)
Data101FormXML <- function(DateFrom, DateTo, CredorgNumber, IndID) {
  url <- "http://www.cbr.ru/CreditInfoWebServ/CreditOrgInfo.asmx"
  # сформировать тело SOAP запроса
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n                <soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <Data101FormXML xmlns=\"http://web.cbr.ru/\">\n                <CredorgNumber>",
    CredorgNumber, "</CredorgNumber>\n                <IndID>", IndID,
    "</IndID>\n                <DateFrom>", DateFrom, "</DateFrom>\n                <DateTo>",
    DateTo, "</DateTo>\n                </Data101FormXML>\n                </soap:Body>\n                </soap:Envelope>")
  name <- "Data101FormXML"
  doc <- GenericSoap2(url = url, body = body, name = name)
  df <- Doc2Df(doc, "FD")
  return(df)

}

# Данные КО. формы 101, без оборотов (как XMLDocument)
Data101Full <- function(DateFrom, DateTo, CredorgNumber, IndCode) {
  url <- "http://www.cbr.ru/CreditInfoWebServ/CreditOrgInfo.asmx"
  # сформировать тело SOAP запроса
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n                <soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <Data101Full xmlns=\"http://web.cbr.ru/\">\n                <CredorgNumber>",
    CredorgNumber, "</CredorgNumber>\n                <IndCode>", IndCode,
    "</IndCode>\n                <DateFrom>", DateFrom, "</DateFrom>\n                <DateTo>",
    DateTo, "</DateTo>\n                </Data101Full>\n                </soap:Body>\n                </soap:Envelope>")
  name <- "Data101Full"
  doc <- GenericSoap2(url = url, body = body, name = name)
  return(doc)

}
# Данные КО. формы 101, без оборотов (как XMLDocument)
Data101FormExXML <- function(DateFrom, DateTo, CredorgNumber, IndID) {
  url <- "http://www.cbr.ru/CreditInfoWebServ/CreditOrgInfo.asmx"
  # сформировать тело SOAP запроса
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n                <soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <Data101FormExXML xmlns=\"http://web.cbr.ru/\">\n                <CredorgNumbers>",
    "<", CredorgNumber, "/>", "</CredorgNumbers>\n                <IndID>",
    IndID, "</IndID>\n                <DateFrom>", DateFrom, "</DateFrom>\n                <DateTo>",
    DateTo, "</DateTo>\n                </Data101FormExXML>\n                </soap:Body>\n                </soap:Envelope>")
  name <- "Data101FormExXML"
  doc <- GenericSoap2(url = url, body = body, name = name)
  return(doc)
}

GetOfficesXML <- function(IntCode) {
  url <- "http://www.cbr.ru/CreditInfoWebServ/CreditOrgInfo.asmx"
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n                 <soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                 <soap:Body>\n                 <GetOfficesXML xmlns=\"http://web.cbr.ru/\">\n                 <IntCode>",
    IntCode, "</IntCode>\n                 </GetOfficesXML>\n                 </soap:Body>\n                 </soap:Envelope>")
  name <- "GetOfficesXML"
  doc <- GenericSoap2(url = url, body = body, name = name)
  df <- Doc2Df(doc, "Offices")
  return(df)
}

# Информация по филиальной сети кредитной орг. по вн.коду (как XML)
GetOfficesByRegionXML <- function(RegCode) {
  url <- "http://www.cbr.ru/CreditInfoWebServ/CreditOrgInfo.asmx"
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n                 <soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                 <soap:Body>\n                 <GetOfficesByRegionXML xmlns=\"http://web.cbr.ru/\">\n                 <IntCode>",
    RegCode, "</IntCode>\n                 </GetOfficesByRegionXML>\n                 </soap:Body>\n                 </soap:Envelope>")
  name <- "GetOfficesByRegionXML"
  doc <- GenericSoap2(url = url, body = body, name = name)
  return(doc)
}


# Данные КО. формы 102, кратко (как XMLDocument) по нескольким КО.

Data101FormExXML <- function(fromDate, ToDate) {
  doc <- CreditOrgFunction("Data101FormXML", fromDate, ToDate, "450000661",
    "438")
  return(doc)
}

# Данные по форме 134 (как XML)
Data134FormFullXML <- function(CredorgNumber, OnDate) {
  h <- RCurl::basicTextGatherer()  #фунция для обработки http-запросов
  url <- "http://www.cbr.ru/CreditInfoWebServ/CreditOrgInfo.asmx"
  # сформировать тело SOAP запроса
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n                <soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <Data134FormFullXML xmlns=\"http://web.cbr.ru/\">\n                <CredorgNumber>",
    CredorgNumber, "</CredorgNumber>\n                <OnDate>", OnDate,
    "</OnDate>\n                </Data134FormFullXML>\n                </soap:Body>\n                </soap:Envelope>")
  name <- "Data134FormFullXML"
  doc <- GenericSoap(url = url, body = body, name = name)

  df <- Doc2Df(doc, "FD")
  return(df)

}

Data134FormFullXML("2306", as.Date("2013-01-01"))

# Данные по форме 135 (как XML)
Data135FormFullXML <- function(CredorgNumber, OnDate) {
  h <- RCurl::basicTextGatherer()  #фунция для обработки http-запросов
  url <- "http://www.cbr.ru/CreditInfoWebServ/CreditOrgInfo.asmx"
  # сформировать тело SOAP запроса
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n                <soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <Data135FormFullXML xmlns=\"http://web.cbr.ru/\">\n                <CredorgNumber>",
    CredorgNumber, "</CredorgNumber>\n                <OnDate>", OnDate,
    "</OnDate>\n                </Data135FormFullXML>\n                </soap:Body>\n                </soap:Envelope>")
  name <- "Data135FormFullXML"
  doc <- GenericSoap(url = url, body = body, name = name)

  df <- Doc2Df(doc, "F135_2")
  return(df)
}

# Bic код в регистрационный номер кред.орг.
BicToRegNumber <- function(bic) {
  url <- "http://www.cbr.ru/CreditInfoWebServ/CreditOrgInfo.asmx"
  # сформировать тело SOAP запроса
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n                <soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <BicToRegNumber xmlns=\"http://web.cbr.ru/\">\n                <BicCode>",
    bic, "</BicCode>\n                </BicToRegNumber>\n                </soap:Body>\n                </soap:Envelope>")
  name <- "BicToRegNumber"
  doc <- GenericSoap2(url = url, body = body, name = name)
  doc <- XML::xmlValue(doc)
  return(doc)
}

BicToRegNumber("044525823")


# Bic код во внутренний код кред.орг.
BicToIntCode <- function(bic) {
  url <- "http://www.cbr.ru/CreditInfoWebServ/CreditOrgInfo.asmx"
  # сформировать тело SOAP запроса
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n                <soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <BicToIntCode xmlns=\"http://web.cbr.ru/\">\n                <BicCode>",
    bic, "</BicCode>\n                </BicToIntCode>\n                </soap:Body>\n                </soap:Envelope>")
  name <- "BicToIntCode"
  doc <- GenericSoap2(url = url, body = body, name = name)
  code <- XML::xmlValue(doc)
  return(code)
}

BicToIntCode(bic)

BicToRegNumber <- function(bic) {
  url <- "http://www.cbr.ru/CreditInfoWebServ/CreditOrgInfo.asmx"
  # сформировать тело SOAP запроса
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n                <soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <BicToRegNumber xmlns=\"http://web.cbr.ru/\">\n                <BicCode>",
    bic, "</BicCode>\n                </BicToRegNumber>\n                </soap:Body>\n                </soap:Envelope>")
  name <- "BicToRegNumber"
  doc <- GenericSoap2(url = url, body = body, name = name)
  doc <- XML::xmlValue(doc)
  return(doc)
}

# Информация о кредитной орг. по вн.коду (как XMLDocument)
CreditInfoByIntCodeXML <- function(InternalCode) {
  url <- "http://www.cbr.ru/CreditInfoWebServ/CreditOrgInfo.asmx"
  # сформировать тело SOAP запроса
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n                <soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                <soap:Body>\n                <CreditInfoByIntCodeXML xmlns=\"http://web.cbr.ru/\">\n                <InternalCode>",
    InternalCode, "</InternalCode>\n                </CreditInfoByIntCodeXML>\n                </soap:Body>\n                </soap:Envelope>")
  name <- "CreditInfoByIntCodeXML"
  doc <- GenericSoap2(url = url, body = body, name = name)
  df <- Doc2Df(doc, "CO")
  return(df)

}

Form101IndicatorsEnumXML <- function() {
  url <- "http://www.cbr.ru/CreditInfoWebServ/CreditOrgInfo.asmx"
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n  <soap:Body>\n    <Form101IndicatorsEnumXML xmlns=\"http://web.cbr.ru/\" />\n  </soap:Body>\n</soap:Envelope>")
  name <- "Form101IndicatorsEnumXML"
  doc <- GenericSoap(url = url, body = body, name = name)
  df <- Doc2Df(doc, "EIND")
  return(df)
}


EnumBIC_XML <- function() {
  url <- "http://www.cbr.ru/CreditInfoWebServ/CreditOrgInfo.asmx"
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n  <soap:Body>\n    <EnumBIC_XML xmlns=\"http://web.cbr.ru/\" />\n  </soap:Body>\n</soap:Envelope>")
  name <- "EnumBIC_XML"
  doc <- GenericSoap(url = url, body = body, name = name)
  df <- Doc2Df(doc, "BIC")
  return(df)
}

# Справочник символов для формы 102 (как XMLDocument)
Form102IndicatorsEnumXML <- function() {
  url <- "http://www.cbr.ru/CreditInfoWebServ/CreditOrgInfo.asmx"
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n  <soap:Body>\n    <Form102IndicatorsEnumXML xmlns=\"http://web.cbr.ru/\" />\n  </soap:Body>\n</soap:Envelope>")
  name <- "Form102IndicatorsEnumXML"
  doc <- GenericSoap(url = url, body = body, name = name)
  df <- Doc2Df(doc, "SIND")
  return(df)
}

# Справочник регионов (как XMLDocument), ver- 18.01.2007
RegionsEnumXML <- function() {
  url <- "http://www.cbr.ru/CreditInfoWebServ/CreditOrgInfo.asmx"
  body <- paste0("<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n  <soap:Body>\n    <RegionsEnumXML xmlns=\"http://web.cbr.ru/\" />\n  </soap:Body>\n</soap:Envelope>")
  name <- "RegionsEnumXML"
  doc <- GenericSoap(url = url, body = body, name = name)
  df <- Doc2Df(doc, "RGID")
  return(df)
}
