#' cbr
#'
#' @name cbr
#' @docType package
#' @author Marcel Salikhov, Boris Demeshev

data("cbr_requests", envir = environment())
data("cbr_requests_unsupported", envir = environment())


#' Convert string with a number in Russian tradition in numeric
#'
#' Convert string with a number in Russian tradition in numeric
#'
#' Russian standards prescribes to use comma as a decimal separator.
#' This function removes spaces and converts string to number.
#'
#' @param x the string with the number
#' @return numeric the number converted from the string
#' @export
#' @examples
#' rus2num('34 345,34')
rus2num <- function(x) {
  x <- gsub(",", ".", x)
  x <- gsub(" ", "", x)
  return(as.numeric(x))
}



#' Convert character in Date format needed to create url
#'
#' Convert character in Date format needed to create url
#'
#' Convert character in Date format needed to create url
#'
#' @param chr the date in character format
#' @return Date the date converted from the character
#' @export
#' @examples
#' chr2date('2016-06-14')
chr2date <- function(chr) {
  chr <- as.Date(chr)
  chr.chr <- as.character(chr, format = "%d.%m.%Y")
}



#' Convert list from xml to data.frame with little changes
#'
#' Convert list from xml to data.frame with little changes
#'
#' Convert list from xml to data.frame with little changes
#'
#' @param url the url of xml list of data
#' @return data.frame 
#' @export
#' @examples
#' extractXML('http://www.cbr.ru/scripts/xml_swap.asp?date_req1=01/12/2002&date_req2=06/12/2002')
extractXML <- function(url) {
  url.xml <- RCurl::getURL(url)
  
  if (url.xml == "") {
    df <- NULL
    warning("Probably no data for selected date. NULL returned.")
  } else {
    cur.list <- XML::xmlToList(url.xml)
    
    # the last element of the list is useless so we drop it, but check
    # beforehand
    cur.list <- droplast(cur.list)
    
    df <- plyr::ldply(cur.list, unlist)
    df <- df[, -1]
    names(df) <- tolower(names(df))
    
  }
  
  return(df)
}



#' Convert excel numeric date encoding to date
#'
#' Convert excel numeric date encoding to date
#'
#' While reading excel files dates are sometimes replaced by their numeric codes.
#' This function recovers original dates from these codes.
#'
#' @param x the vector of numeric date codes
#' @return the date
#' @export
#' @examples
#' excel2date(12345)
excel2date <- function(x) {
  ans <- as.Date(as.POSIXct((x - 25569) * 86400, tz = "GMT", origin = "1970-01-01"))
  return(ans)
}



#' Drops last entry in the list x if this entry is character
#'
#' Drops last entry in the list x if this entry is character
#'
#' Drops last entry in the list x if this entry is character
#'
#' @param x the list
#' @return list with omitted last element
#' @examples
#' a <- list(x = 5, y = 'last')
#' cbr:::droplast(a)
droplast <- function(x) {
  n.obs <- length(x)
  if (class(x[[n.obs]]) == "character")
    x <- x[-n.obs]
  return(x)
}



#' Actual currency prices from Central Bank of Russia on date.
#'
#' Actual currency prices from Central Bank of Russia on date.
#'
#' Actual currency prices from Central Bank of Russia on date.
#'
#' @param ondate the date of actual currency prices, character or Date
#' @return data.frame with actual currency prices on date from cbr.ru
#' @export
#' @examples
#' df <- cbr_cur_ondate(ondate = '2016-06-14')
cbr_cur_ondate <- function(ondate = "2016-06-14") {
  
  # onDate = '2016-06-14'
  
  url <- paste0("http://www.cbr.ru/scripts/XML_daily_eng.asp?date_req=", chr2date(ondate))
  
  df <- extractXML(url)
  
  if (is.null(df)) {
    message("No data on that date.")
  } else {
    colnames(df)[colnames(df) == ".attrs.id"] <- "id"
    df$value <- rus2num(df$value)
  }
  
  return(df)
}



#' Historical currency prices from Central Bank of Russia.
#'
#' Historical currency prices from Central Bank of Russia.
#'
#' Historical currency prices from Central Bank of Russia.
#'
#' @param currency internal Central Bank currency code starting with the letter 'R'
#' @param from the first day of the time interval, character or Date
#' @param to the last day of the time interval, character or Date
#' @return data.frame with historical currency prices from cbr.ru
#' @export
#' @examples
#' df <- cbr_currency(currency = 'R01120',
#'   from = '1993-01-05', to = '2013-01-09')
#' # 'R01120' --- Burundi frank :)
cbr_currency <- function(currency = "R01120",
                         from = "1993-01-05", to = "2013-09-18") {
  
  # currency <- 'R01120'
  # from <- '1993-01-05'
  # to <- '2013-09-18'
  
  url <- paste0("http://www.cbr.ru/scripts/XML_dynamic.asp?date_req1=",
                chr2date(from), "&date_req2=", chr2date(to), "&VAL_NM_RQ=", currency)
  
  df <- extractXML(url)
  
  if (is.null(df)) {
    message("No data on that date.")
  } else {
    names(df) <- c("units", currency, "date")
    
    # correct type
    df$units <- rus2num(df$units)
    df[, 2] <- rus2num(df[, 2])
    df$date <- as.Date(df$date, format = "%d.%m.%Y")
  }
  
  return(df)
}



#' Credit institutions balances on correspondent accounts with Bank of Russia.
#'
#' Credit institutions balances on correspondent accounts with Bank of Russia.
#'
#' Credit institutions balances on correspondent accounts with Bank of Russia.
#'
#' @param from the first day of the time interval, character or Date
#' @param to the last day of the time interval, character or Date
#' @return data.frame with credit institutions balance from cbr.ru
#' @export
#' @examples
#' df <- cbr_balances(from = '2012-01-01', to = '2013-01-09')
cbr_balances <- function(from = "2007-01-01", to = "2013-01-01") {
  
  # from='2007-01-01' to='2013-01-01'
  
  url <- paste0("http://www.cbr.ru/scripts/XML_ostat.asp?date_req1=",
                chr2date(from), "&date_req2=", chr2date(to))
  
  df <- extractXML(url)
  
  if (is.null(df)) {
    message("No data on that date.")
  } else {
    # correct type
    names(df) <- c("russia", "moscow", "date")
    df$date <- as.Date(df$date, format = "%d.%m.%Y")
    
  }
  
  return(df)
}



#' Historical metal prices from Central Bank of Russia.
#'
#' Historical metal prices from Central Bank of Russia.
#'
#' Historical metal prices from Central Bank of Russia.
#'
#' @param from the first day of the time interval, character or Date
#' @param to the last day of the time interval, character or Date
#' @return data.frame with historical metal prices from cbr.ru
#' @export
#' @examples
#' df <- cbr_metal(from = '2012-01-01', to = '2013-01-09')
cbr_metal <- function(from = "2007-01-01", to = "2013-01-01") {
  
  # from='2007-01-01' to='2013-01-01'
  
  url <- paste0("http://www.cbr.ru/scripts/xml_metall.asp?date_req1=",
                chr2date(from), "&date_req2=", chr2date(to))
  
  df <- extractXML(url)
  
  if (is.null(df)) {
    message("No data on that date.")
  } else {
    # correct type
    names(df) <- c("buy", "sell", "date", "code")
    
    df$date <- as.Date(df$date, format = "%d.%m.%Y")
    df$code <- as.factor(df$code)
    df$buy <- rus2num(df$buy)
    df$sell <- rus2num(df$sell)
  }
  
  return(df)
}



#' Historical credit rates on Interbank Credit Market.
#'
#' Historical credit rates on Interbank Credit Market.
#'
#' Historical credit rates on Interbank Credit Market.
#'
#' @param from the first day of the time interval, character or Date
#' @param to the last day of the time interval, character or Date
#' @return data.frame with historical credit rates from cbr.ru
#' @export
#' @examples
#' df <- cbr_credit_rates(from = '2012-01-01', to = '2013-01-09')
cbr_credit_rates <- function(from = "2007-01-01", to = "2013-01-01") {
  
  # from='2007-01-01' to='2013-01-01'
  
  url <- paste0("http://www.cbr.ru/scripts/xml_mkr.asp?date_req1=",
                chr2date(from), "&date_req2=", chr2date(to))
  
  df <- extractXML(url)
  
  if (is.null(df)) {
    message("No data on that date.")
  } else {
    # correct type
    names(df) <- c("daily", "weekly", "monthly", "3months", "halfyear", "yearly", "date", "code")
    
    df$date <- as.Date(paste0(substring(df$date, first = 1, last = 2), 
                              ".", substring(df$date, first = 4, last = 5), 
                              ".", substring(df$date, first = 7, last = 10)), format = "%d.%m.%Y")
  }
  
  return(df)
}



#' Historical swap prices from Central Bank of Russia.
#'
#' Historical swap prices from Central Bank of Russia.
#'
#' Historical swap prices from Central Bank of Russia.
#'
#' @param from the first day of the time interval, character or Date
#' @param to the last day of the time interval, character or Date
#' @return data.frame with historical swap prices from cbr.ru
#' @export
#' @examples
#' df <- cbr_swap(from = '2012-01-01', to = '2013-01-09')
cbr_swap <- function(from = "2007-01-01", to = "2013-01-01") {
  
  # from='2007-01-01' to='2013-01-01'
  
  url <- paste0("http://www.cbr.ru/scripts/xml_swap.asp?date_req1=", 
                chr2date(from), "&date_req2=", chr2date(to))
  
  df <- extractXML(url)
  
  if (is.null(df)) {
    message("No data on that date.")
  } else {
    # correct type
    names(df) <- c("datebuy", "datesell", "baserate", "sd", "tir", "ir",
                   "euro")
    
    # types
    df$baserate <- rus2num(df$baserate)
    df$ir <- rus2num(df$ir)
    df$tir <- rus2num(df$tir)
    df$sd <- rus2num(df$sd)
    
    df$euro <- factor(df$euro)
    df$datebuy <- as.Date(df$datebuy, format = "%d.%m.%Y")
    df$datesell <- as.Date(df$datesell, format = "%d.%m.%Y")
  }
  
  return(df)
}



#' Release Prices of the Bank of Russia for Investment Coins.
#'
#' Release Prices of the Bank of Russia for Investment Coins.
#'
#' Release Prices of the Bank of Russia for Investment Coins.
#'
#' @param from the first day of the time interval, character or Date
#' @param to the last day of the time interval, character or Date
#' @return data.frame with historical release prices for investment coins from cbr.ru
#' @export
#' @examples
#' df <- cbr_coins(from = '2012-01-01', to = '2013-01-09')
cbr_coins <- function(from = "2007-01-01", to = "2013-01-01") {
  
  # from='2007-01-01' to='2013-01-01'
  
  url <- paste0("http://www.cbr.ru/scripts/XMLCoinsBase.asp?date_req1=",
                chr2date(from), "&date_req2=", chr2date(to))
  
  df <- extractXML(url)
  
  if (is.null(df)) {
    df <- NULL
    message("No data on that date.")
  } else {
    # correct type
    names(df) <- c("date", "price", "nominal", "metaltype", "metalquantity", "coinid")
    
    df$date <- as.Date(paste0(substring(df$date, first = 1, last = 2), 
                              ".", substring(df$date, first = 4, last = 5), 
                              ".", substring(df$date, first = 7, last = 10)), format = "%d.%m.%Y")
    
    df$price <- rus2num(df$price)
    df$metalquantity <- rus2num(df$metalquantity)
  }
  
  return(df)
}



#' Create body of html-request
#'
#' Create body of html-request
#'
#' Create body of html-request
#'
#' @param body the body of html-request
#' @param name the name of variable from http://www.cbr.ru/secinfo/secinfo.asmx
#' @export
#' @return XML-tree
#' @examples
#' body <- paste0(
#' "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n
#' <soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" 
#'   xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" 
#'   xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n
#'     <soap:Body>\n                
#'       <AuctionsXML xmlns=\"http://web.cbr.ru/\">\n
#'         <DateFrom>2015-01-01</DateFrom>\n
#'         <DateTo>2015-06-01</DateTo>\n                
#'       </AuctionsXML>\n
#'     </soap:Body>\n                
#' </soap:Envelope>")
#' cbr_get_body(body, "AuctionsXML")
cbr_get_body <- function(body, name) {
  h <- RCurl::basicTextGatherer()
  url <- "http://cbr.ru/secinfo/secinfo.asmx"
  
  HeaderFields <- c(Accept = "text/xml", Accept = "multipart/*", SOAPAction = paste("\"http://web.cbr.ru/",
                                                                                    name, "\"", sep = ""), `Content-Type` = "text/xml; charset=utf-8")
  RCurl::curlPerform(url = url, httpheader = HeaderFields, postfields = body,
                     writefunction = h$update)
  response <- h$value()
  doc <- XML::xmlInternalTreeParse(response)
  return(doc)
}



#' Convert list from xml to data.frame without changes
#'
#' Convert list from xml to data.frame without changes
#'
#' Convert list from xml to data.frame without changes
#'
#' @param name the name of variable from http://www.cbr.ru/secinfo/secinfo.asmx
#' @param firstDate start of period, Date, as.Date("2000-01-01") by default
#' @param secondDate end of period, Date, Sys.Date() by default
#' @export
#' @return data.frame
#' @examples
#' df <- cbr_security_info("AuctionsXML")
cbr_security_info <- function(name, 
                              firstDate = as.Date("2000-01-01"), 
                              secondDate = Sys.Date()) {
  
  type <- cbr_requests[cbr_requests$name == name, "type"]
  
  if (length(type) == 0) {
    stop("The name ", name, " is not supported by Central Bank or by this function.") 
  }
  
  if (type == "DateFromTo") {
    
    # сформировать тело SOAP запроса
    body <- paste0(
      "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n
      <soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n
      <soap:Body>\n                
      <", name, " xmlns=\"http://web.cbr.ru/\">\n
      <DateFrom>", firstDate, "</DateFrom>\n
      <DateTo>", secondDate, "</DateTo>\n                
      </", name, ">\n
      </soap:Body>\n                
      </soap:Envelope>")
  } 
  
  if (type == "OnToDate") {
    
    body <- paste0(
      "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n
      <soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n
      <soap:Body>\n                
      <", name, " xmlns=\"http://web.cbr.ru/\">\n
      <OnDate>", firstDate, "</OnDate>\n
      <ToDate>", secondDate, "</ToDate>\n                
      </", name, ">\n
      </soap:Body>\n                
      </soap:Envelope>")
  } 
  
  if (type == "dt") {
    
    body <- paste0(
      "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n
      <soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n                
      <soap:Body>\n                
      <", name, " xmlns=\"http://web.cbr.ru/\">\n
      <dt>", secondDate, "</dt>\n
      </", name, ">\n
      </soap:Body>\n
      </soap:Envelope>")
  } 
  
  if (type == "OnDate") {
    
    body <- paste0(
      "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n
      <soap:Envelope xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">\n
      <soap:Body>\n
      <", name, " xmlns=\"http://web.cbr.ru/\">\n
          <OnDate>", secondDate, "</OnDate>\n           
        </", name, ">\n
      </soap:Body>\n
    </soap:Envelope>")
  } 
  
  doc <- cbr_get_body(body, name)
  
  node <- cbr_requests[cbr_requests$name == name, "node"]
  xml_node <- XML::getNodeSet(doc, paste0("//", node))
  df <- XML::xmlToDataFrame(doc, nodes = xml_node)
  
  if (nrow(df) == 0) { 
    warning("Probably no data on that date.")
    return(NULL)
  }
  
  return(df)
  } 



