#' cbr
#'
#' @name cbr
#' @docType package
#' @author Marcel Salikhov, Boris Demeshev


# todo CBR:

# cur day coins deposit interbank


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

  from <- as.Date(from)
  to <- as.Date(to)

  from.chr <- as.character(from, format = "%d.%m.%Y")
  to.chr <- as.character(to, format = "%d.%m.%Y")

  url <- paste0("http://www.cbr.ru/scripts/XML_dynamic.asp?date_req1=",
    from.chr, "&date_req2=", to.chr, "&VAL_NM_RQ=", currency)

  url.xml <- RCurl::getURL(url)

  if (url.xml == "") {
    df <- NULL
    warning("Probably no data for selected period. NULL returned.")
  } else {
    cur.list <- XML::xmlToList(url.xml)

    # the last element of the list is useless so we drop it, but check
    # beforehand
    cur.list <- droplast(cur.list)

    df <- plyr::ldply(cur.list, unlist)

    # drop `record` and currency name
    df <- df[, c(2, 3, 4)]

    names(df) <- c("units", currency, "date")

    # correct type
    df[, 1] <- rus2num(df[, 1])
    df[, 2] <- rus2num(df[, 2])
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

  # this two lines allow both Date and string format for `from` and `to`
  from <- as.Date(from)
  to <- as.Date(to)

  from.chr <- as.character(from, format = "%d.%m.%Y")
  to.chr <- as.character(to, format = "%d.%m.%Y")

  url <- paste0("http://www.cbr.ru/scripts/xml_metall.asp?date_req1=",
    from.chr, "&date_req2=", to.chr)

  url.xml <- RCurl::getURL(url)

  metal.list <- XML::xmlToList(url.xml)

  # the last element of the list is useless so we drop it, but check
  # beforehand
  metal.list <- droplast(metal.list)

  df <- plyr::ldply(metal.list, unlist)

  df <- df[, -1]  # drop 'record' word
  names(df) <- c("buy", "sell", "date", "code")

  df$date <- as.Date(df$date, format = "%d.%m.%Y")
  df$code <- as.factor(df$code)
  df$buy <- rus2num(df$buy)
  df$sell <- rus2num(df$sell)

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

  # this two lines allow both Date and string format for `from` and `to`
  from <- as.Date(from)
  to <- as.Date(to)

  from.chr <- as.character(from, format = "%d.%m.%Y")
  to.chr <- as.character(to, format = "%d.%m.%Y")

  url <- paste0("http://www.cbr.ru/scripts/xml_swap.asp?date_req1=", from.chr,
    "&date_req2=", to.chr)

  url.xml <- RCurl::getURL(url)

  swap.list <- XML::xmlToList(url.xml)

  # the last element of the list is useless so we drop it, but check
  # beforehand
  swap.list <- droplast(swap.list)

  df <- plyr::ldply(swap.list, unlist)

  # drop `record`
  df <- df[, -1]

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

  return(df)
}
