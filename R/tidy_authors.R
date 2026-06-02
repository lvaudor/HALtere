#' Description
#' @param custom_name a publications list custom name
#' @param data_dir the path to data directory
#' @param method method to shorten names. Defaults to "shortest".
#' @return a tibble
#' @export
#' @examples
#' data=extract_publications("BIOEENVIS", nmax=200)
#' data_ref_authors=tidy_ref_authors(data)
tidy_ref_authors=function(custom_name, data_dir="data", method="shortest"){
  datapath=glue::glue("{data_dir}/{custom_name}/publications.RDS")
  data=readRDS(datapath)
  # get the info about names, ids and affiliation of each author of each ref
  dat=data %>%
    tidyr::unnest(cols=c("authIdHasPrimaryStructure_fs")) %>%
    dplyr::mutate(auth_and_aff=authIdHasPrimaryStructure_fs) %>%
    tidyr::separate(auth_and_aff,sep="_JoinSep_", into=c("auth","affiliation")) %>%
    tidyr::separate(auth, sep="_FacetSep_",into=c("id_auth","auth")) %>%
    tidyr::separate(id_auth, sep="-",into=c("id_HAL","id_internal")) %>%
    tidyr::separate(affiliation,sep="_FacetSep_",into=c("id_affiliation","affiliation"))
  if(method=="longest"){foptimum=max}
  if(method=="shortest"){foptimum=min}

  # Homogenize author names through time to get most used if possible
  tib_authors_names=tibble::tibble(auth=data %>%
                                     dplyr::pull(authIdFullName_fs) %>%
                                     unique() %>%
                                     unlist()) %>%
    tidyr::separate(auth, sep="_FacetSep_",into=c("id_internal","name_var"))
  tib_authors_names_with_internal_id=tib_authors_names %>%
    dplyr::filter(id_internal!=0) %>%
    dplyr::arrange(id_internal) %>%
    unique() %>%
    dplyr::mutate(name_length=stringr::str_length(name_var)) %>%
    dplyr::group_by(id_internal) %>%
    dplyr::mutate(n=dplyr::n(),
                  name_length_optimum=foptimum(name_length)) %>%
    dplyr::mutate(name=name_var[name_length==name_length_optimum][1]) %>%
    dplyr::mutate(name=dplyr::case_when(is.na(name)~name_var,
                                        TRUE~name)) %>%
    dplyr::ungroup() %>%
    dplyr::select(id_internal,name,name_var) %>%
    dplyr::group_by(id_internal) %>%
    dplyr::top_n(n=1)

  # Replace author names with homogenized author names in ref_authors
  dat=dat %>%
    dplyr::left_join(tib_authors_names_with_internal_id %>%
                       dplyr::select(id_internal,name) %>%
                       unique(),by=c("id_internal"),
                     relationship = "many-to-many") %>%
    dplyr::mutate(name=dplyr::case_when(is.na(name)~auth,
                                        TRUE~name)) %>%
    dplyr::select(-starts_with("auth"))

  # For each year and name keep main affiliation
  dat_aff=dat  %>%
    dplyr::select(producedDateY_i,affiliation,name) %>%
    unique() %>%
    dplyr::group_by(producedDateY_i,name) %>%
    dplyr::mutate(n=dplyr::n()) %>%
    dplyr::arrange(n=desc(n)) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::select(-n)%>%
    dplyr::ungroup()
  # In case there is a year with no affiliation, keep major affiliation through all times
  dat_aff_maj=dat_aff %>%
    dplyr::select(affiliation,name) %>%
    unique() %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(n=dplyr::n()) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::select(-n) %>%
    dplyr::rename(affmaj=affiliation) %>%
    dplyr::ungroup()
  dat=dat %>%
    dplyr::select(-affiliation) %>%
    unique() %>%
    dplyr::left_join(dat_aff,by=c("producedDateY_i","name"),relationship = "many-to-many") %>%
    dplyr::left_join(dat_aff_maj,by=c("name"),relationship = "many-to-many") %>%
    dplyr::mutate(affiliation=dplyr::case_when(is.na(affiliation)~affmaj,
                                               TRUE~affiliation)) %>%
    dplyr::select(-affmaj)
  dat=dat %>%
    dplyr::mutate(name_simplified=stringr::str_extract(name,"(?<=\\s).*$"),
                  affiliation_simplified=stringr::str_replace_all(affiliation,"[^A-Z0-9]*",""))

  saveRDS(dat,glue::glue("{data_dir}/{custom_name}/data_ref_authors.RDS"))
  return(dat)
}


#' Produces a simplified table of ref_authors
#' @param publications a tibble produced with the extract_publications() function
#' @param data_ref_authors a tibble produced with the tidy_ref_authors() function
#' @return a tibble with simplified ref_authors information
#' @export
#' @examples
#' data=extract_publications("BIOEENVIS", nmax=200)
#' data_ref_authors=tidy_ref_authors(data)
#' show_ref_authors(publications=data, data_ref_authors=data_ref_authors)
show_ref_authors=function(publications, data_ref_authors){
  data_ref_authors= data_ref_authors %>%
    tidyr::unite("authors",all_of(c("name","affiliation")),sep=" (") %>%
    dplyr::mutate(authors=paste0(authors,")")) %>%
    dplyr::select(id_ref,authors) %>%
    dplyr::group_by(id_ref) %>%
    tidyr::nest() %>%
    dplyr::mutate(authors=purrr::map_chr(data, ~paste0(.$authors,collapse="; "))) %>%
    dplyr::select(-data) %>%
    dplyr::ungroup() %>%
    dplyr::select(id_ref,authors)
  publis=publications %>%
    dplyr::mutate(title=dplyr::case_when(translate_title_s==TRUE~paste0(title_s," (translated)"),TRUE~title_s),
                  keywords=dplyr::case_when(translate_keyword_s==TRUE~paste0(keyword_s," (translated)"),TRUE~keyword_s),
                  abstract=dplyr::case_when(translate_keyword_s==TRUE~paste0(abstract_s," (translated)"),TRUE~abstract_s)) %>%
    dplyr::select(id_ref,
                  title,
                  journal=journalTitle_s,
                  docType=docType_s,
                  year=producedDateY_i,
                  keywords,
                  abstract,
                  text) %>%
    dplyr::filter(year>=input$years[1],
                  year<=input$years[2]) %>%
    dplyr::left_join(data_ref_authors) %>%
    unique()

}


#' Description
#' @param data_ref_authors a tibble produced with the tidy_ref_authors() function
#' @return a tibble with all title words
#' @export
#' @examples
#' data=extract_publications("EVS_UMR5600", nmax=50)
#' data_ref_authors=tidy_ref_authors(data)
#' data_words=tidy_words(data_)
tidy_words=function(custom_name, data_dir="data"){
  datapath=glue::glue("{data_dir}/{custom_name}/data_ref_authors.RDS")
  data_ref_authors = readRDS(datapath)
  data(lexicon_en)
  res=data_ref_authors %>%
    tidytext::unnest_tokens(output=word,input=title_s,token="words")%>%
    dplyr::left_join(lexicon_en,by="word") %>%
    dplyr::filter(is.na(type)| type %in% c("adj","ver","nom")) %>%
    dplyr::mutate(lemma_completed=dplyr::case_when(is.na(lemma)~word,
                                                   TRUE~lemma)) %>%
    dplyr::select(halId_s,
                  producedDateY_i,
                  name,
                  affiliation,
                  name_simplified,
                  affiliation_simplified,
                  word,
                  type,
                  lemma_completed)

  saveRDS(res,glue::glue("{data_dir}/{custom_name}/data_words.RDS"))
  return(res)
}

#' Get groups stats from publications and groups-relative stats
#' @param custom_name a publications list custom name
#' @param data_dir the path to data directory
#' @param method method to shorten names. Defaults to "shortest"
#' @param type whether to group by "people" or "labs"
#' @return a tibble
#' @export
#' @examples
#' data_labs=tidy_groups("BIOEENVIS",type="labs")
tidy_groups=function(custom_name, data_dir="data",method="shortest", type="people"){
  datapath=glue::glue("{data_dir}/{custom_name}/data_ref_authors.RDS")
  data_ref_authors = readRDS(datapath)
  if(type=="labs"){
    data_ref_authors=data_ref_authors %>%
    dplyr::mutate(name=affiliation,
                  name_simplified=affiliation_simplified)
    }
  dat_groups=data_ref_authors %>%
    dplyr::group_by(name,name_simplified,affiliation, producedDateY_i, docType_s) %>%
    dplyr::summarise(nrefs=dplyr::n_distinct(id_ref),
                     .groups="drop")

  #  res=data_ref_authors %>%
  #   tidytext::unnest_tokens(output=word,input=title_s,token="words")%>%
  #   dplyr::left_join(lexicon_en,by="word") %>%
  #   dplyr::filter(is.na(type)| type %in% c("adj","ver","nom")) %>%
  #   dplyr::mutate(lemma_completed=dplyr::case_when(is.na(lemma)~word,
  #                                                  TRUE~lemma))
  # spec_unique=tidy_specificities(res %>% dplyr::filter(!is.na(lemma)),
  #                                cat1=name,
  #                                cat2=lemma) %>%
  #   dplyr::arrange(name,desc(spec)) %>%
  #   dplyr::group_by(name) %>%
  #   tidyr::nest() %>%
  #   dplyr::mutate(data=purrr::map(data,~.x[1,])) %>%
  #   tidyr::unnest(cols=c(data)) %>%
  #   dplyr::ungroup()
  # dat_groups=dat_groups %>%
  #   dplyr::left_join(spec_unique %>%
  #                      dplyr::select(name,lemma,spec),
  #                    by="name") %>%
  #   dplyr::mutate(lemma=dplyr::case_when(is.na(lemma)~name,
  #                                        TRUE~lemma))

  saveRDS(dat_groups,glue::glue("{data_dir}/{custom_name}/data_{type}.RDS"))
  return(dat_groups)
}
