#' clean date
#'
#' Clean date data
#' @param x
#' @param format valid input includes 'monthday'
#' @keywords date
#' @export
#' @examples
#' clean_date()

clean_date <- function(x,format = 'monthday') {
  function(x){
    require(stringr)
    
    input <-  c('%m/%d/%Y','%m-%d-%Y','%Y/%m/%d','%Y-%m-%d','%B %d %Y','%B %d, %Y','%B/%d/%Y','%B %d')
    output <- c(monthday ='%B %d')
  
    x <- remove_ws(x)
    x[is_valid(as.Date(x,tryFormats = input,optional=TRUE))] <- str_replace(format(as.Date(x,tryFormats = input,optional=TRUE),output[format]),'\\s[0]',' ')

    return(x)
  }
}

#' valid date
#'
#' Identify date data that does not meet Niche's expectations 
#' @param x
#' @param format valid input includes 'all','monthday'
#' @param na_to_false
#' @keywords date
#' @export
#' @examples
#' valid_date()

valid_date <- function(x, format = 'all', allow = NULL, na_to_false = FALSE) {
  function(x){
    require(stringr)
    
    # logic
    index <- 1:length(x)
    index[index][!is_valid(x[index])] <- 0
    
    if(format == 'monthday'){
      index[index][is.na(format(as.Date(x[index],'%B %d'),'%B %d'))] <- 0
      index[index][str_detect(x[index], "\\s[0]")] <- 0
    } else {
      index[index][x[index] == clean_date()(x[index]) & is.na(format(as.Date(x[index],'%B %d'),'%B %d'))] <- 0
    } 
    
    if(!is.null(allow)){index[which(x %in% allow)] <- which(x %in% allow)}
    
    # identify invalid values
    if(na_to_false == FALSE){invalid <- which(!is_valid(x))}
    
    x <- return_values(index,invalid)
    
    return(x)
  }
}



