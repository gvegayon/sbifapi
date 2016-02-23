
#' Returns available indicators
#'
#' @export
sbif_indicators <- function() {
  list(
    what=c('dolar', 'euro', 'ipc', 'uf', 'utm'),
    frame=c('fixed', 'anteriores', 'posteriores')
  )
}

#' Generic function for the GET method of the API
#'
#' @param what Object to retrieve (see Details)
#' @param frame For the particular date, before that date, or after that date
#' @param apikey API key
#' @param formato Format of the returning data, can be either JSON or XML
#' @param callback Callback function for JSON data
#' @param Year Year
#' @param Month Month
#' @param Day Day
#' @param retry Number of times to retry the call to the API
#'
#' @details Currently, the available objects are
#' \itemize{
#'  \item{what}{dolar},
#'  \item frame fixed, before, after
#'  year, year/month, year/month/dias/, posteriores/year,
#'  posteriores/year/month, posteriores/year/month/day, and the same with
#'  anteriores
#'  \item
#' }
#' @references
#' SBIF API Documentation \url{http://api.sbif.cl/index.html}
#' @export
#' @examples
#' \dontrun{
#' # Retrieving dollar info for 2015 and 2016
#' dollar2015 <- sbifapi("dolar", Year=2015, apikey="mykey")
#' dollar2016 <- sbifapi("dolar", Year=2016, apikey="mykey")
#' }
sbifapi <- function(what='dolar', frame='fixed',
                    apikey, formato='JSON', callback=NULL,
                    Year=lubridate::year(Sys.Date()),
                    Month=NULL,
                    Day=NULL,
                    retry=1) {

  # Checkin the parameters
  if (!(what %in% sbif_indicators()$what)) stop('Invalid indicator.')

  if (!(frame %in% sbif_indicators()$frame)) stop('Invalid frame.')

  # Building the URI
  if (frame=='fixed') {
    uri <- paste(c(Year, Month), collapse='/')
    if (length(Day)) uri <-paste0(uri, '/dias/', Day)
    frame <- NULL
  }
  else uri <- paste(c(Year, Month, Day), collapse='/')

  # Checking what kind of query it is
  uri <- paste(c(what, frame, uri), collapse='/')

  uri <- paste0('http://api.sbif.cl/api-sbifv3/recursos_api/', uri, '?')

  req   <- structure(NULL, class='error')
  retry <- retry + 1
  while (inherits(req, 'error') & retry) {
    req <- tryCatch(httr::GET(uri, query=list(apikey=apikey, formato=formato)),
                    error=function(e) e)

    retry <- retry - 1
  }

  status <- httr::status_code(req)
  if (status != 200) {
    # print(req)
    warning('Problemas en la consulta. Revise el error en http://api.sbif.cl/api-codigos-de-error.html')
    httr::stop_for_status(req)
  }

  req <- unlist(httr::content(req, type='application/json'), FALSE)

  output <- data.frame(
    Valor = as.numeric(gsub(',','.',gsub('\\.','', sapply(req, '[[', 'Valor')))),
    Fecha = as.Date(lubridate::ymd(sapply(req, '[[', 'Fecha'))),
    stringsAsFactors = FALSE
    )

  # Processing and returing the output
  return(output)

}


# sbifapi('uf', frame='fixed')
# sbifapi('uf', Month='02', frame='fixed')
# sbifapi('uf', Month='02', Day='01', frame='fixed')
#
# sbifapi('uf', frame='anteriores')
# sbifapi('uf', Month='02', frame='anteriores')
# sbifapi('uf', Month='02', Day='01', frame='anteriores')
#
# sbifapi('uf', frame='posteriores')
# sbifapi('uf', Month='02', frame='posteriores')
# sbifapi('uf', Month='02', Day='01', frame='posteriores')
#
# rm(list=ls())
