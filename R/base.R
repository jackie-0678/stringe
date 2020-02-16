#' Is Valid Values
#'
#' This function selects only non-null, non-na values, non-blank, and non text 'NA' values.
#' @param x 
#' @keywords valid
#' @export
#' @examples
#' is_valid()

is_valid <- function(x) {
  
  x <- ifelse(x == 0, TRUE, x)
  x <- ifelse(x == 'NANA', FALSE, x)
  x <- ifelse(is.na(x) | is.null(x),FALSE,x)
  x <- ifelse(x == 'NA' | x == '', FALSE, x)
  x <- ifelse(x == 'N/A' | x == 'n/a' , FALSE, x)
  x <- ifelse(x != FALSE, TRUE, FALSE)
  
  return(x)
}

#' return values
#'
#' returns values of x that are within the provided numeric index
#' @param x
#' @param index numeric list of list elements to return
#' @param invalid 
#' @keywords return
#' @export
#' @examples
#' return_values()

return_values <- function(index, invalid = NULL) {
  try(if(!all(invalid %in% 0:length(index)) | !is.numeric(index)) stop("invalid arg must be a valid numeric vector for the index arg", call. = FALSE))
  
  # assign t/f
  index[index > 0] <- TRUE
  index[index == 0] <- FALSE

  # identify invald entries as NA
  if(!is.null(invalid)){index[invalid] <- NA}
  index <- as.logical(index)

  return(index)
}

#' Finds bad urls.
#'
#' Returns false for bad urls
#' @param x A url
#' @keywords valid
#' @export
#' @examples
#' valid_url_status()

valid_url_status <- function(x) {
  require(httr)
  
  web_status <- ifelse(try(HEAD(x, timeout(10))$status_code < 404, silent = T) == T, TRUE,FALSE)
  
  if(web_status == FALSE) {
    web_status <- ifelse(try(GET(x,timeout(15))$status_code < 404, silent = T) == T,TRUE, FALSE)
  }
  
  return(web_status)
}

#' common regex 
#'
#' Returns common regex text
#' @param 
#' @keywords regex
#' @export
#' @examples
#' get_regex()

get_regex <- list(
  all_text = function(){return('A-Za-z0-9\\W')},
  alph_digit = function(){return('A-Za-z0-9')},
  digit = function(){return('\\d')},
  alph = function(){return('A-Za-z')},
  alph_ = function(){return('\\w')},
  alph_ws = function(){return('A-Za-z\\s')},
  alph_digit_ws = function(){return('A-Za-z0-9\\s')}
)

#' before after
#'
#' comparison tool for evaluating original values and returned function values
#' @param x A vector
#' @param f function
#' @keywords before; after; comparison
#' @export
#' @examples
#' before_after()

before_after <- function(x,FUN,...){
  require(knitr)
  
  p <- as.data.frame(x)
  p$new <- FUN(x, ...)
  p <- p[p$x != p$new,]
  
  kable(p)
}


#' Removes whitespace.
#'
#' Removes any leading or trailing whitespace. Removes more than one consecutive whitespace.
#' @param x A vector
#' @keywords whitespace
#' @export
#' @examples
#' remove_ws()

remove_ws <- function(x){
  require(stringr)
  
  x <- str_trim(x)
  x <- str_replace_all(x,"  "," ")
  
  return(x)
}


#' Updates text case.
#'
#' Changes the case of input text.
#' @param x A vector
#' @param set_case arguments can be "lower","upper" or "title"
#' @keywords case
#' @export
#' @examples
#' text_case()

text_case <- function(x, set_case = NULL){
  require(stringr)
  
  if(is.null(set_case)){
    x
  } else if(set_case =='lower'){
    x <- tolower(x)
  } else if(set_case == 'upper'){
    x <- toupper(x)
  } else if(set_case == 'title'){
    x <- totitle(x)
  }
  
  return(x)
}

#' Title case text.
#'
#' Capitalizes the first letter of each word. If all caps, will lower the string first.
#' @param x A vector
#' @param width provide max length of string for whitespace padding purposes
#' @keywords valid
#' @export
#' @examples
#' totitle()

totitle <- function(x, width = 100) {
  require(stringr)
  
  x <- ifelse(!str_detect(x,"[^A-Z\\s\\d\\']"), tolower(x), x)
  
  if(any(str_detect(x, "^[a-z]|\\s[a-z]"))){
    x <- str_pad(x, width, pad= " ")
    locate_lower <- unique(unlist(str_extract_all(x,"\\s[a-z]" )))
    uppercase <- toupper(unlist(locate_lower))
    names(uppercase) <- locate_lower
    x <- str_replace_all(x, uppercase)
    x <- str_trim(x)
  }
  
  return(x)
}

