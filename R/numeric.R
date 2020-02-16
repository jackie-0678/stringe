#' clean numeric
#'
#' Clean numeric data
#' @param x
#' @param decimals
#' @keywords numeric
#' @export
#' @examples
#' clean_numeric()

clean_numeric <- function(x, decimals = 2) {
  require(stringr)

  x <- remove_ws(x)
  x <- str_replace_all(x,'[^\\d\\.]','')
  x[is_valid(suppressWarnings(as.numeric(x)))] <- formatC(as.numeric(x[is_valid(suppressWarnings(as.numeric(x)))]), digits = decimals, format = 'f')

  return(x)
}


#' valid numeric
#'
#' Identify numeric data that does not meet expectations
#' @param x
#' @param decimals
#' @param minn
#' @param maxx
#' @param natofalse
#' @keywords numeric
#' @export
#' @examples
#' valid_numeric()

valid_numeric <- function(x, minn = 0, maxx = .Machine$integer.max, decimals = 2, natofalse = FALSE) {
  require(stringr)

  # logic
  index <- 1:length(x)
  index[index][!is_valid(x[index])] <- 0

  ifelse(decimals == 0 | is.null(decimals), dot <- "", dot<-"\\.")

  index[index][str_detect(x[index],paste0("[^\\d",dot,"]"))] <- 0
  index[index][!str_detect(x[index],paste0("(\\d",dot,"\\d{",decimals,"})$"))] <- 0
  index[index][floor(as.numeric(x[index])) > .Machine$integer.max] <- 0
  index[index][as.numeric(x[index]) < minn | as.numeric(x[index]) > maxx] <- 0

  # identify invalid values
  if(natofalse == FALSE){
  	invalid <- which(!is_valid(x))
  	} else{ invalid <- NULL}

  x <- return_values(index,invalid)

  return(x)
}
