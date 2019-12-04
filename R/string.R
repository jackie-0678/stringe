#' clean string
#'
#' Clean string data
#' @param x
#' @param set_regex
#' @param set_case valid input can be 'title','lower','upper'
#' @param set_recode input should be a named vector. ex: "remove char" = "replace char"
#' @param FUN an additionanl function call
#' @keywords string
#' @export
#' @examples
#' clean_string()

clean_string <- function(x, set_case = NULL, set_recode = NULL, FUN = NULL, ...){
  require(stringr)

  if(!is.null(set_recode)){x <- str_replace_all(x, set_recode)}
  x <- remove_ws(x)
  if(!is.null(set_case)){x <- text_case(x,set_case)}
  if(!is.null(FUN)){x <- FUN(x, ...)}

  return(x)
}

#' valid string
#'
#' Identify string data that does not meet Niche's expectations 
#' @param x
#' @param distinct
#' @param na_to_false
#' @keywords distinct string
#' @export
#' @examples
#' valid_string()

valid_string <- function(x, distinct_vals = NULL, set_regex = NULL, na_to_false = FALSE){
  require(stringr)

  index <- 1:length(x)
  index[index][!is_valid(x[index])] <- 0

  if(!is.null(set_regex)){index[index][!str_detect(x[index], paste0("(",set_regex,")"))] <- 0}
  if(!is.null(distinct_vals)){index[index][!(x[index] %in% distinct_vals)] <- 0}

  # identify invalid values
  if(na_to_false == FALSE){invalid <- which(!is_valid(x))}

  x <- return_values(index,invalid)

  return(x)
}
