# Retry utility ------------------------------------------------------------

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
  NULL
}

# HAL slice extraction -----------------------------------------------------

extract_ith_slice_of_publications <- function(
    i,
    query,
    nmax
) {

  url <- "https://api.archives-ouvertes.fr/search/"

  start <- i * 100

  nrows <- min(100, max(0, nmax - start))

  if (nrows == 0) return(tibble::tibble())

  params <- list(
    q = query,
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

  res <- httr::GET(url, query = params)

  data_tmp <- httr::content(res, as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON() |>
    purrr::pluck("response", "docs")

  message(glue::glue("Extracted {start} to {start + nrows} ({query})"))

  tibble::as_tibble(data_tmp)
}

# Main extraction ----------------------------------------------------------

extract_publications <- function(
    custom_name,
    query,
    data_dir = "data_HALtere",
    nmax = Inf
) {

  data_dir <- file.path(data_dir, custom_name)

  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }

  data <- tibble::tibble()

  i <- 0
  nr <- 100

  # pagination safe loop
  while (nr > 0 && nrow(data) < nmax) {

    data_tmp <- retry_skip(
      extract_ith_slice_of_publications(
        i = i,
        query = query,
        nmax = nmax
      ),
      max_try = 3,
      wait = 3
    )

    if (is.null(data_tmp)) {
      stop("Failed to retrieve data from HAL API.")
    }

    if (nrow(data_tmp) == 0) break

    data <- dplyr::bind_rows(data, data_tmp)

    nr <- nrow(data_tmp)
    i <- i + 1
  }

  # safe deduplication
  data <- dplyr::distinct(data, halId_s, .keep_all = TRUE)

  old_file <- file.path(data_dir, "publications.RDS")

  # -----------------------------------------------------------------------
  # merge with existing dataset if needed
  # -----------------------------------------------------------------------

  if (file.exists(old_file)) {

    data_old <- readRDS(old_file)

    data_old_to_cross <- data_old |>
      dplyr::transmute(
        halId_s,
        date_old = modifiedDate_tdate,
        old_fr_abstract_s = fr_abstract_s,
        old_fr_title_s = fr_title_s,
        old_fr_keyword_s = fr_keyword_s,
        old_en_abstract_s = en_abstract_s,
        old_en_title_s = en_title_s,
        old_en_keyword_s = en_keyword_s
      ) |>
      dplyr::mutate(in_old = TRUE)

    data <- data |>
      dplyr::left_join(data_old_to_cross, by = "halId_s") |>
      dplyr::filter(is.na(in_old) | modifiedDate_tdate > date_old) |>
      dplyr::select(-in_old, -date_old)

    n_updates <- dplyr::n_distinct(data$halId_s)

    message(glue::glue("{n_updates} publications to extract/update."))

    if (n_updates == 0) {
      return(data_old)
    }

  } else {

    data <- data |>
      dplyr::mutate(
        old_fr_title_s = NA,
        old_fr_keyword_s = NA,
        old_fr_abstract_s = NA,
        old_en_title_s = NA,
        old_en_keyword_s = NA,
        old_en_abstract_s = NA
      )
  }

  # -----------------------------------------------------------------------
  # translation flags
  # -----------------------------------------------------------------------

  is_not_equal <- function(x, y) {
    is.na(x) | is.na(y) | x != y
  }

  dat <- data |>
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
        is_not_equal(old_fr_title_s, fr_title_s) &
        !is.na(fr_title_s),

      translate_keyword_s =
        is.na(en_keyword_s) &
        is_not_equal(old_fr_keyword_s, fr_keyword_s) &
        !is.na(fr_keyword_s),

      translate_abstract_s =
        is.na(en_abstract_s) &
        is_not_equal(old_fr_abstract_s, fr_abstract_s) &
        !is.na(fr_abstract_s)
    ) |>
    dplyr::mutate(
      title_s = old_en_title_s,
      keyword_s = old_en_keyword_s,
      abstract_s = old_en_abstract_s
    )

  # -----------------------------------------------------------------------
  # translation steps
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

  dat <- dat |>
    dplyr::mutate(
      abstract_s = complete_with_translated_texts(
        en_abstract_s,
        fr_abstract_s,
        translate_abstract_s,
        long = TRUE
      )
    )

  # -----------------------------------------------------------------------
  # final assembly
  # -----------------------------------------------------------------------

  dat <- dat |>
    tidyr::unite("text", keyword_s, abstract_s, title_s, remove = FALSE) |>
    dplyr::select(-starts_with("old_"))

  # merge safely with existing file
  if (file.exists(old_file)) {

    data_old <- readRDS(old_file)

    dat <- dplyr::bind_rows(data_old, dat) |>
      dplyr::distinct(halId_s, .keep_all = TRUE)
  }

  saveRDS(dat, old_file)

  dat
}
