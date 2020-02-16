#' valid url
#'
#' Identify url data that does not meet expectations 
#' @param x A url
#' @param natofalse 
#' @keywords valid
#' @export
#' @examples
#' valid_url()

valid_url <- function(x, natofalse = FALSE) {
  require(stringr)

  index <- 1:length(x)
  index[index][!is_valid(x[index])] <- 0
  index[index][!(substr(x[index],1,4) == "http")] <- 0
  index[index][str_detect(x[index],"//$")] <- 0
  index[index][!vapply(x[index],valid_url_status,logical(1))] <- 0

  # identify invalid values
  if(natofalse == FALSE){
  	invalid <- which(!is_valid(x))
  	} else{ invalid <- NULL}

  x <- return_values(index,invalid)

  return(x)
}

