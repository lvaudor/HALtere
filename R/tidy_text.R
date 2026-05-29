#' Produces data_text, which is derived from a publications tibble
#' @param collection a collection name
#' @param data_dir the path to data directory
#' @return a tibble
#' @export
#' @examples
#' data_text=tidy_text("BIOEENVIS", data=data_dir,column="text")
tidy_text=function(collection,data_dir="data",column){
  data=readRDS(glue::glue("{data_dir}/{collection}/publications.RDS"))
  string_column=column
  column=rlang::sym(column)
  res=data %>%
    dplyr::filter(!is.na(title_s)) %>%
    tidytext::unnest_tokens(input=!!column,output=word, token="words")
  data(lexicon_en)
  res=res %>%
    dplyr::left_join(lexicon_en,by="word") %>%
    dplyr::filter(is.na(type)| type %in% c("adj","ver","nom")) %>%
    # if word is not in lexicon keep it as_is
    dplyr::mutate(lemma_completed=dplyr::case_when(is.na(lemma)~word,
                                                    TRUE~lemma)) %>%
    dplyr::filter(is.na(type)| type %in% c("adj","ver","nom")) %>%
    dplyr::select(id_ref,docType_s,producedDateY_i,word,lemma,lemma_completed,type) %>%
    dplyr::group_by(id_ref) %>%
    dplyr::mutate(n=purrr::map_int(lemma_completed,~length(is.na))) %>%
    dplyr::ungroup()

  saveRDS(res,
          glue::glue("{data_dir}/{collection}/text.RDS"))

  return(res)
}
