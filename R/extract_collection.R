#' Retry to run a code
#' @param max_try maximum number of trials. Defaults to 3
#' @param wait the number of seconds to wait between trials. Defaults to 0.
#' @return NULL
#' @export
retry_skip <- function(expr, max_try = 3, wait = 0){
  for (i in seq_len(max_try)) {
    result <- tryCatch({eval(expr)},
                       error = function(e) {
                         message(sprintf("Erreur à la tentative %d : %s", i, e$message))
                         return(NULL)
                       }
    )
    if (!is.null(result)) return(result)
    if (wait > 0) Sys.sleep(wait)
  }
  message("Toutes les tentatives ont échoué.")
  return(NULL)
}


#' Extract the publication slice going from i*100 to i*100+100
#'
#' @param i the slice number
#' @param identifier identifier of the collection/person/lab
#' @param hal_variable the HAL variable used to retrieve items. Can be collCode_s (for collections), authIdHal_s (for person)
#' @param nmax maximum number of publications to extract
#'
#' @return a data.frame containing a slice of publications
#' @export

extract_ith_slice_of_publications <- function(
    i,
    identifier,
    hal_variable = "collection",
    nmax=+Inf
) {
  url <- "https://api.archives-ouvertes.fr/search/"
  start <- i * 100
  nrows <- min(c(100, nmax - start),na.rm = TRUE)

  # Construction de la requête HAL
  q <- glue::glue("{hal_variable}:({identifier})")

  # Paramètres de la requête
  params <- list(
    q = q,
    rows = nrows,
    start = start,
    wt = "json",

    fl = paste(
      c(
        "authIdHasPrimaryStructure_fs",
        "authIdFullName_fs",
        "producedDateY_i",
        "journalTitle_s",
        "docType_s",
        "fr_keyword_s",
        "en_keyword_s",
        "en_title_s",
        "fr_title_s",
        "en_abstract_s",
        "fr_abstract_s",
        "language_s",
        "halId_s",
        "modifiedDate_tdate"
      ),
      collapse = ","
    )
  )
  # Requête API
  data_tmp <- httr::GET(url,query = params) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>%
    .$response %>%
    .$docs
  message(
    glue::glue(
      "Extracted results {start} to {start + nrows} ({hal_variable}: {identifier})"
    )
  )
  return(data_tmp)
}

#' Creates file publications.RDS as direct export from HAL API in data_dir
#' @param collection name of a HAL collection
#' @param data_dir path to data directory. Defaults to "data".
#' @param n_max the maximum number of items to retrieve (this argument is useful for testing purposes). Defaults to 200.
#' @param hal_variable the HAL variable used to retrieve items. Can be collCode_s (for collections), authIdHal_s (for person)
#' @return a tibble
#' @export
#' @examples
#' publis=extract_collection("EVS_UMR5600", nmax=30)
extract_collection=function(collection, data_dir="data", nmax=+Inf, hal_variable="collCode_s"){
      data_dir=glue::glue("{data_dir}/{hal_variable}_{collection}")
      if(!file.exists(data_dir)){dir.create(data_dir)}
      #URL de l'API HAL
      data=tibble::tibble()
      nr=1000
      i=0
      # The query is sent by batches of 100 items, until data reaches nmax
      while(nr>=100 & nrow(data)<nmax){
        retry_skip({data_tmp=extract_ith_slice_of_publications(i,
                                                               collection,
                                                               nmax,
                                                               hal_variable=hal_variable)},
                   max_try=3,wait=3)
        data=dplyr::bind_rows(data,data_tmp)
        nr=nrow(data_tmp)
        i=i+1
      }
      #remove repeated publications
      data=unique(data)
      if(file.exists(glue::glue("{data_dir}/publications.RDS"))){
        data_old=readRDS(glue::glue("{data_dir}/publications.RDS"))
        # Texts will be translated from fr to en only when these have been modified
        data_old_to_cross=data_old %>%
          dplyr::select(halId_s,
                        date_old=modifiedDate_tdate,
                        old_fr_abstract_s=fr_abstract_s,
                        old_fr_title_s=fr_title_s,
                        old_fr_keyword_s=fr_keyword_s,,
                        old_en_abstract_s=en_abstract_s,
                        old_en_title_s=en_title_s,
                        old_en_keyword_s=en_keyword_s) %>%
          dplyr::mutate(in_old=TRUE)
        data=data %>%
          dplyr::left_join(data_old_to_cross,
                           by="halId_s") %>%
          dplyr::filter(is.na(in_old)|modifiedDate_tdate>date_old) %>%
          dplyr::select(-in_old,-date_old)
        n_updates=length(unique(data$halId_s))
        print(paste0(n_updates," publications to extract / update."))
        if(n_updates==0){return(data_old)}
      }else{
        data=data %>%
          dplyr::mutate(old_fr_title_s=NA,
                        old_fr_keyword_s=NA,
                        old_fr_abstract_s=NA,
                        old_en_title_s=NA,
                        old_en_keyword_s=NA,
                        old_en_abstract_s=NA)
      }
      is_not_equal=function(x,y){
        prop1=(x!=y)
        prop2=is.na(x!=y)
        return(prop1|prop2)
      }
      dat= data %>%
        dplyr::mutate(id_ref=1:dplyr::n()) %>%
        dplyr::mutate(dplyr::across(c(en_title_s,fr_title_s,
                                      en_abstract_s,fr_abstract_s,
                                      en_keyword_s,fr_keyword_s),replace_null_with_na)
                      ) %>%
        dplyr::mutate(translate_title_s=dplyr::case_when(is.na(en_title_s) & is_not_equal(old_fr_title_s,fr_title_s) & !is.na(fr_title_s)~TRUE,
                                                         TRUE~FALSE),
                      translate_keyword_s=dplyr::case_when(is.na(en_keyword_s) & is_not_equal(old_fr_keyword_s,fr_keyword_s) & !is.na(fr_keyword_s)~TRUE,
                                                          TRUE~FALSE),
                      translate_abstract_s=dplyr::case_when(is.na(en_abstract_s) & is_not_equal(old_fr_abstract_s,fr_abstract_s) & is.na(fr_abstract_s)~TRUE,
                                                            TRUE~FALSE)) %>%
        dplyr::mutate(title_s=old_en_title_s,
                      keyword_s=old_en_keyword_s,
                      abstract_s=old_en_abstract_s)
        n=dat %>% dplyr::filter(translate_title_s==TRUE) %>% nrow()
        print(paste("Translating",n,"titles."))
        dat=dat %>%
          dplyr::mutate(title_s=complete_with_translated_texts(en_title_s,fr_title_s,translate_title_s))
        n=dat %>% dplyr::filter(translate_keyword_s==TRUE) %>% nrow()
        print(paste("Translating",n,"keywords lists."))
        dat=dat %>%
          dplyr::mutate(keyword_s=complete_with_translated_texts(en_keyword_s,fr_keyword_s,translate_keyword_s))
        n=dat %>% dplyr::filter(translate_abstract_s==TRUE) %>% nrow()
        print(paste("Translating",n,"abstracts."))
        dat=dat %>%
          dplyr::mutate(abstract_s=complete_with_translated_texts(en_abstract_s,fr_abstract_s,translate_abstract_s, long=TRUE))
        dat=dat %>%
        tidyr::unite("text",keyword_s,abstract_s,title_s,remove=FALSE) %>%
          dplyr::select(-starts_with("old_"))

      if(file.exists(glue::glue("{data_dir}/publications.RDS"))){
        data_old=readRDS(glue::glue("{data_dir}/publications.RDS"))
        dat=rbind(data_old,dat)
      }
      saveRDS(dat,glue::glue("{data_dir}/publications.RDS"))
      return(dat)
}
