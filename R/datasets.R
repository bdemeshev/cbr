#' The data.frame of supported securities requests
#'
#' A data.frame containing the list of securities requests 
#' for Central Bank of Russia supported by this package.
#'
#' @format data.frame with 37 rows and 4 variables
#' \describe{
#'   \item{name}{security name}
#'   \item{node}{short-cut for parsing XML tree}
#'   \item{type}{type of request structure}
#'   \item{status}{indicator whether the request is checked}
#' }
#' @source manual check
"cbr_requests"


#' The data.frame of unsupported securities requests
#'
#' A data.frame containing the list of securities requests 
#' for Central Bank of Russia unsupported by this package.
#'
#' @format  data.frame with 19 rows and 4 variables
#' \describe{
#'   \item{name}{security name}
#'   \item{node}{short-cut for parsing XML tree}
#'   \item{type}{type of request structure}
#'   \item{status}{indicator whether the request is checked}
#' }
#' @source manual check
"cbr_requests_unsupported"

