#' Translate text if necessary and place it in the data tibble
#' @param text_to_complete the vector of texts to complete with translated texts (the elements such that whether_to_translate is FALSE are left unchanged)
#' @param text_to_translate the text to translate
#' @param whether_to_translate whether or not to translate the text to English (can be FALSE if the text is already in English for instance).
#' @param long whether the text is long (polyglotr's google_translate_long() function will be used if long is TRUE)
#' @return a tibble
#' @export
#' @examples
#' tib=tibble::tibble(text_original=c("Do not translate","Oui celui-là, oui", "Allons-y"),
#' translate=c(FALSE,TRUE,TRUE))
#' library(dplyr)
#' tib %>%
#'   mutate(text_trans=complete_with_translated_texts(text_original,text_original,translate))
complete_with_translated_texts=function(text_to_complete,text_to_translate,whether_to_translate, long=FALSE){

  resulting_text=text_to_complete
  ind=which(whether_to_translate==TRUE & !is.na(text_to_translate))
  if(long==TRUE){translate_function=polyglotr::google_translate_long_text}else{translate_function=polyglotr::google_translate}
  if(length(ind)>1){
    for (i in 1:length(ind)){
      if(floor((i-1)/100)==(i-1)/100){
      Sys.sleep(0.5)
      print(paste("Translating items ", i,"to",min(length(ind),i+99)))
      }
      result=translate_function(text_to_translate[ind[i]],
                         source_language="fr",
                         target_language="en") %>%
        list() %>%
        replace_null_with_na()
      resulting_text[ind[i]]=result
    }
  }
  return(resulting_text)
}
