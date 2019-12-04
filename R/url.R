#' valid url
#'
#' Identify url data that does not meet Niche's expectations 
#' @param x A url
#' @param na_to_false 
#' @keywords valid
#' @export
#' @examples
#' valid_url()

valid_url <- function(x, na_to_false = FALSE) {
  function(x){
    require(stringr)
    
    index <- 1:length(x)
    index[index][!is_valid(x[index])] <- 0
    index[index][!(substr(x[index],1,4) == "http")] <- 0
    index[index][str_detect(x[index],"//$")] <- 0
    index[index][!vapply(x[index],valid_url_status,logical(1))] <- 0
    
    # identify invalid values
    if(na_to_false == FALSE){invalid <- which(!is_valid(x))}
    
    x <- return_values(index,invalid)
    
    return(x)
  }
}

