#' Retry to run a code
#' @param max_try maximum number of trials. Defaults to 3
#' @param wait the number of seconds to wait between trials. Defaults to 0.
#' @return NULL
#' @export
retry_skip <- function(expr, max_try = 3, wait = 0) {
  expr <- substitute(expr)
  for (i in seq_len(max_try)) {
    result <- tryCatch(
      eval(expr, envir = parent.frame()),
      error = function(e) {
        message(sprintf("Erreur à la tentative %d : %s", i, e$message))
        NULL
      }
    )
    if (!is.null(result)) return(result)
    if (wait > 0) Sys.sleep(wait)
  }
  message("Toutes les tentatives ont échoué.")
  return(NULL)
}

#' Extract the publication slice going from i*100 to i*100+100
#' @param i the slice number
#' @param query the HAL query used to retrieve items.
#' @param identifier identifier of the collection/person/lab
#' @param nmax maximum number of publications to extract
#' @return a data.frame containing a slice of publications
#' @export
extract_ith_slice_of_publications <- function(
    i,
    query,
    nmax
) {
  url <- "https://api.archives-ouvertes.fr/search/"
  start <- i * 100
  nrows <- min(100, max(0, nmax - start))
  if (nrows == 0) return(tibble::tibble())
  required_columns=c(
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
  )
  params <- list(
    q = query,
    rows = nrows,
    start = start,
    wt = "json",
    fl = paste(required_columns,collapse = ",")
  )
  res <- httr::GET(url, query = params)
  data_tmp <- httr::content(res, as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON() |>
    purrr::pluck("response", "docs") %>%
    tibble::as_tibble()
  missing_cols <- setdiff(required_columns, names(data_tmp))
  data_tmp <- data_tmp %>%
    dplyr::mutate(
      !!!setNames(rep(list(NA), length(missing_cols)), missing_cols)
    )
  message(glue::glue("Extracted {start} to {start + nrows} ({query})"))
  return(data_tmp)
}

#' Creates file publications.RDS as direct export from HAL API in data_dir
#'  @param custom_name name to give to the list of publications. If NA (default) the directory will be named after the used HAL identifier.
#'  @param query the HAL query used to retrieve items. This query should be built using the HAL API syntax.
#'  @param data_dir path to data directory. Defaults to "data_HALtere".
#'  @param n_max the maximum number of items to retrieve (this argument is useful for testing purposes). Defaults to 200.
#'  @return a tibble
#'  @export
#'  @examples
#'  test1=extract_publications(custom_name="test1",query= 'collCode_s:"EVS_UMR5600"',  nmax=30)
#'  test2=extract_publications(custom_name="test2",query= 'collCode_s:"EVS_UMR5600"',  nmax=30)
#'  publis=extract_publications(custom_name="Rencontres_R",
#'                              query = 'title_autocomplete:"Rencontres R"',  nmax=+Inf)
#'

extract_publications <- function(
    custom_name,
    query,
    data_dir = "data_HALtere",
    nmax = Inf,
    previous_data_maxdate=NULL
) {
  # Create directory if needed
  data_dir <- file.path(data_dir, custom_name)
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }
  # Check previous data
  previous_file <- file.path(data_dir, "publications.RDS")
  if (file.exists(previous_file)) {
    previous_data <- readRDS(previous_file)
    previous_data_maxdate=max(previous_data$modifiedDate_tdate, na.rm=TRUE)
    query=paste0(query, " AND modifiedDate_tdate:[", previous_data_maxdate, " TO NOW]")
  }

  data_result <- tibble::tibble()
  i <- 0
  nr <- 100
  # pagination safe loop
  while (nr > 0 && nrow(data_result) < nmax) {
    data_tmp <- retry_skip(
      extract_ith_slice_of_publications(
        i = i,
        query = query,
        nmax = nmax
      ),
      max_try = 3,
      wait = 3
    )
    if (nrow(data_tmp) == 0) break
    data_result <- dplyr::bind_rows(data_result, data_tmp)
    nr <- nrow(data_tmp)
    i <- i + 1
  }

  if(nrow(data_result)==0 & file.exists(previous_file)){
    warning("Failed to retrieve new recordings from HAL API.")
  }
  if(nrow(data_result)==0 & !file.exists(previous_file)){
    stop("Failed to retrieve any recording from HAL API.")
  }
  # -----------------------------------------------------------------------
  # translation flags
  # -----------------------------------------------------------------------
  dat <- data_result |>
    dplyr::mutate(id_ref = dplyr::row_number()) |>
    dplyr::mutate(
      dplyr::across(
        c(en_title_s, fr_title_s,
          en_abstract_s, fr_abstract_s,
          en_keyword_s, fr_keyword_s),
        replace_null_with_na
      )
    ) |>
    dplyr::mutate(
      translate_title_s =
        is.na(en_title_s) &
        !is.na(fr_title_s),

      translate_keyword_s =
        is.na(en_keyword_s) &
        !is.na(fr_keyword_s),

      translate_abstract_s =
        is.na(en_abstract_s) &
        !is.na(fr_abstract_s)
    )

  # -----------------------------------------------------------------------
  # Translation
  # -----------------------------------------------------------------------

  message(glue::glue("Translating {sum(dat$translate_title_s)} titles"))

  dat <- dat |>
    dplyr::mutate(
      title_s = complete_with_translated_texts(
        en_title_s, fr_title_s, translate_title_s
      )
    )

  message(glue::glue("Translating {sum(dat$translate_keyword_s)} keywords"))
  dat <- dat |>
    dplyr::mutate(
      keyword_s = complete_with_translated_texts(
        en_keyword_s, fr_keyword_s, translate_keyword_s
      )
    )

  message(glue::glue("Translating {sum(dat$translate_abstract_s)} abstracts"))
  # dat <- dat |>
  #   dplyr::mutate(
  #     abstract_s = complete_with_translated_texts(
  #       en_abstract_s,
  #       fr_abstract_s,
  #       translate_abstract_s,
  #       long=TRUE
  #     )
  #   )
  dat=dat %>% dplyr::mutate(abstract_s=NA)
  for (i in 1:nrow(dat)){
    dat$abstract_s[i]=complete_with_translated_texts(
      dat$en_abstract_s[i],
      dat$fr_abstract_s[i],
      dat$translate_abstract_s[i],
      long=TRUE
    )
  }
  # Remove translation flags and old columns, and create a "text" column that unites title, abstract and keywords (this will be useful for the next steps of the project)
  dat <- dat |>
    tidyr::unite("text", keyword_s, abstract_s, title_s, remove = FALSE) |>
    dplyr::select(-starts_with("translate_"))

  # Merge safely with existing file
  if (file.exists(previous_file)) {
    dat <- dplyr::bind_rows(previous_data, dat) %>%
      dplyr::arrange(halId_s,desc(modifiedDate_tdate)) %>%
      dplyr::group_by(halId_s) %>%
      dplyr::mutate(rank=1:dplyr::n()) %>%
      dplyr::filter(rank==1) %>%
      dplyr::select(-rank) %>%
      dplyr::ungroup()
  }
  saveRDS(dat, previous_file)
  return(dat)
}
