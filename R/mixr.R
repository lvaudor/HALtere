#' Returns tibble with frequencies of cat (lemma or word for instance) in data
#' @param mydf a tibble
#' @param cat words or lemmas, for instance
#' @param top_freq how many items by category (filter based on frequency) should be kept. If not provided (the default) everything is kept.
#' @param min_freq which is the minimum specificity for an item to be kept. If not provided (the default) everything is kept.
#' @return a tibble of cat frequencies
#' @export
#' @examples
#' mydf <- tibble::tibble(txt=janeaustenr::prideprejudice) %>%
#'   tidytext::unnest_tokens(word,txt)
#' tidy_frequencies(mydf, word, min_freq=200)
tidy_frequencies <- function(mydf,
                             cat,
                             top_freq=NA,
                             min_freq=NA){
  qcat=rlang::enquo(cat)
  freq_data <- mydf %>%
    dplyr::group_by(!!qcat) %>%
    dplyr::summarise(freq=dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(.data$freq))
  if(!is.na(top_freq)){
    freq_data <- freq_data %>%
      dplyr::top_n(top_freq)
  }
  if(!is.na(min_freq)){
    freq_data <- freq_data %>%
      dplyr::filter(.data$freq>=min_freq)
  }
  return(freq_data)
}

#' Returns a tibble with specificities according to two crossed categories.
#' @param mydf a tibble
#' @param cat1 a factor corresponding to words or lemmas
#' @param cat2 a category
#' @param top_spec how many items by category (filter based on specificity) should be kept. If not provided (the default) everything is kept.
#' @param min_spec which is the minimum specificity for an item to be kept. If not provided (the default) everything is kept.
#' @return tibble with additional columns cat1, cat2, spec
#' @export
#' @examples
#'  mydf=dplyr::bind_rows(
#'          tibble::tibble(txt=janeaustenr::prideprejudice,
#'          novel="Pride and Prejudice"),
#'          tibble::tibble(txt=janeaustenr::sensesensibility,
#'          novel="Sense and Sensibility")) %>%
#'       tidytext::unnest_tokens(word,txt)
#'  tidy_specificities(mydf,
#'                     cat1=word,
#'                     cat2=novel)

tidy_specificities=function(mydf,
                            cat1,
                            cat2,
                            top_spec=NA,
                            min_spec=NA){
  qcat1 <- rlang::enquo(cat1)
  qcat2 <- rlang::enquo(cat2)
  vcat1=mydf %>%
    dplyr::select(!!qcat1) %>%
    dplyr::pull(1)
  vcat2=mydf %>%
    dplyr::select(!!qcat2) %>%
    dplyr::pull(1)
  freqs=mydf %>%
    dplyr::group_by(!!qcat1,!!qcat2) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::select(cat1=!!qcat1,
                  cat2=!!qcat2,
                  n)
  spe=textometry::specificities(table(vcat1,vcat2))
  spe=dplyr::bind_cols(cat1=row.names(spe),
                       tibble::as_tibble(spe,
                                         .name_repair="minimal")) %>%
    tidyr::gather("cat2","spec",
                  -cat1) %>%
    dplyr::left_join(freqs, by=c("cat1","cat2"))
  if(!is.na(top_spec)){
    spe <- spe %>%
      dplyr::group_by(cat2) %>%
      dplyr::slice_max(order_by=.data$spec,n=top_spec,with_ties=FALSE) %>%
      dplyr::ungroup()
  }
  if(!is.na(min_spec)){
    spe <- spe %>%
      dplyr::filter(.data$spec>min_spec)
  }
  spe <- spe %>%
    dplyr::arrange(dplyr::desc(.data$spec)) %>%
    purrr::set_names(c(colnames(dplyr::select(mydf,!!qcat1,!!qcat2)),
                       "spec","n"))
  return(spe)
}

#' Returns a plot showing frequencies of cat (lemma or word for instance) in data
#' @param df_freq a tibble with frequencies of cat
#' @param cat words or lemmas, for instance
#' @param frequency frequency of cat
#' @param scale type of transformation (if any) to apply to the x-axis (e.g. "log","sqrt",etc.)
#' @return a plot
#' @export
#' @examples
#' mydf <- tibble::tibble(txt=janeaustenr::prideprejudice) %>%
#'     tidytext::unnest_tokens(word,txt)
#' df_freq <- tidy_frequencies(mydf, word, min_freq=500)
#' plot_frequencies(df_freq,
#'                  cat=word,
#'                  frequency=freq)
plot_frequencies=function(df_freq, cat, frequency, scale=NA, fill=NULL, fill_fixed="grey50"){
  fill=rlang::enquo(fill)
  cat=rlang::enquo(cat)
  frequency=rlang::enquo(frequency)
  p = ggplot2::ggplot(df_freq,
                      ggplot2::aes(x = forcats::fct_reorder({{cat}},{{frequency}}),
                                   y = {{frequency}}))
  if(!rlang::quo_is_null(fill)){
    p=p + ggplot2::geom_col(ggplot2::aes(fill={{fill}}),
                            alpha=0.5)
  }else{
    p=p + ggplot2::geom_col(fill=fill_fixed,
                            alpha=0.5)
  }
  p = p +
    ggplot2::coord_flip() +
    ggplot2::geom_text(ggplot2::aes(x={{cat}}, y = 0,
                                    label = {{cat}}),
                       hjust = 0) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank()) +
    ggplot2::xlab(rlang::enquo(cat))
  return(p)
}
#' Plot specificities of cat1 (lemma or word) according to cat2 (categories)
#' @param df_spec a tibble with specifictities of cat1 according to cat2
#' @param cat1 words or lemmas
#' @param cat2 categories
#' @return a plot
#' @export
#' @examples
#' mydf=dplyr::bind_rows(tibble::tibble(txt=janeaustenr::prideprejudice,
#'                                      novel="Pride and Prejudice"),
#'                       tibble::tibble(txt=janeaustenr::sensesensibility,
#'                                      novel="Sense and Sensibility")) %>%
#'  tidytext::unnest_tokens(word,txt)
#' df_spec <- tidy_specificities(mydf,
#'                               cat1=word,
#'                               cat2=novel,
#'                               min_spec=5)
#' plot_specificities(df_spec,
#'                    cat1=word,
#'                    cat2=novel)
plot_specificities=function(df_spec, cat1, cat2){
  cat1 <- rlang::enquo(cat1)
  cat2 <- rlang::enquo(cat2)
  df_spec <- df_spec %>%
    dplyr::group_by(!!cat2)
  df_spec <- df_spec %>%
    dplyr::arrange(!!cat2,.data$spec)%>%
    dplyr::mutate(id=1:length(.data$spec))
  p=ggplot2::ggplot(df_spec,
                    ggplot2::aes(x=df_spec$id,
                                 y=df_spec$spec,
                                 fill=factor(!!cat2)))+
    ggplot2::geom_bar(stat="identity", alpha=0.5)+
    ggplot2::geom_text(ggplot2::aes(label=!!cat1, y=0), hjust=0)+
    ggplot2::coord_flip()+
    ggplot2::facet_wrap(ggplot2::vars(!!cat2), scales="free")+
    ggplot2::scale_x_discrete(breaks=NULL)+
    ggplot2::theme(legend.position="none")+
    ggplot2::labs(x=cat1,y="specificity score")
  return(p)
}

