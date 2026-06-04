#' Replaces nulls in a list with NA
#' @param list_with_nulls a list comprising nulls
#' @return list comprising NAs
#' @export
#' @examples
#' replace_null_with_na(list(0,33,"a", NULL,"b",NA,NULL))
replace_null_with_na=function(list_with_nulls){
  result=list_with_nulls %>%
    purrr::map(~ifelse(is.null(.x[[1]]),NA,.x[[1]])) %>%
    unlist()
  return(result)
}
