#' Description
#' @param data a tibble produced with the tidy_ref_authors() function and containing lines only for one reference
#' @return a tibble
#' @examples
#' data=extract_publications("BIOEENVIS", nmax=+Inf)
#' data_ref_authors=tidy_ref_authors(data)
#' data_groups=tidy_groups(data_ref_authors)
#' cross_by_group(data_ref_authors %>% dplyr::filter(id_ref==1))
cross_by_group=function(data_ref_authors_oneref, var_groups="name"){
  values=unique(data_ref_authors_oneref[[var_groups]])
  values_crossed=tidyr::expand_grid(values,values)  %>%
    dplyr::mutate(val1=values...1,val2=values...2)
  return(values_crossed)
}
#' Cross people or labs to quantify number of co-publications
#' @param custom_name a publications list custom name
#' @param data_dir the path to data directory
#' @param method method to shorten names. Defaults to "shortest"
#' @param type whether to group by "people" or "labs"
#' @return a tibble which crosses people or labs to tally number of collaborations
#' @export
#' @examples
#' HALtere::cross("BIOEENVIS","data_HALtere", type="people")
cross=function(custom_name, data_dir, type="people"){
  # Retrieve data_ref_authors
  data_ref_authors=readRDS(glue::glue("{data_dir}/{custom_name}/data_ref_authors.RDS"))
  # Retrieve data_groups
  data_groups=readRDS(glue::glue("{data_dir}/{custom_name}/data_{type}.RDS"))

  if(all(data_groups$name==data_groups$affiliation)){var_groups="affiliation"}else{var_groups="name"}

  crossed_data=data_ref_authors%>%
    dplyr::group_by(id_ref,producedDateY_i,docType_s) %>%
    tidyr::nest() %>%
    dplyr::mutate(data=purrr::map(data,
                                  .f=~HALtere:::cross_by_group(.x,var_groups))) %>%
    tidyr::unnest(cols=c("data")) %>%
    dplyr::group_by(val1,val2,docType_s,producedDateY_i) %>%
    dplyr::summarise(nlinks=dplyr::n(),.groups="drop") %>%
    dplyr::ungroup()
    data_groups=data_groups %>%
        dplyr::group_by(docType_s,producedDateY_i,affiliation,name,name_simplified) %>%
        dplyr::summarise(nrefs=sum(nrefs),.groups="drop")
    crossed_data=crossed_data %>%
      dplyr::left_join(data_groups,
                       by=c("val1"="name",
                            "producedDateY_i"="producedDateY_i",
                            "docType_s"="docType_s"))

    saveRDS(crossed_data,glue::glue("{data_dir}/{custom_name}/crossed_{type}.RDS"))
  return(crossed_data)
}
