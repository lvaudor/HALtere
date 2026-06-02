#' Prepare line of HALtere directories-related data
#'
#' @param query HAL query used to retrieve publications
#' @param custom_name name to give to the list of publications.
#' If NA (default) the directory will be named after the query.
#'
#' @return one line of dataset regarding a list of publications
#' @export
#'
#' @examples
#' prepare_HALtere_directory(
#'   query='authIdHal_s:"lise-vaudor"',
#'   custom_name="Lise Vaudor"
#' )

prepare_HALtere_directory <- function(
    query,
    custom_name = NA
) {

  if (is.na(custom_name)) {
    custom_name <- query
  }

  data_line <- tibble::tibble(
    custom_name = custom_name,
    query = query
  )

  return(data_line)
}



#' Get all HALtere data
#'
#' @param custom_name name of the publication list
#' @param query HAL query used to retrieve publications
#' @param data_dir root directory where HALtere data sub-directories are kept
#'
#' @return summary of retrieved datasets
#' @export
#'
#' @examples
#' prepare_all_data(
#'   custom_name = "Lise Vaudor",
#'   query = 'authIdHal_s:"lise-vaudor"'
#' )

prepare_all_data <- function(
    custom_name,
    query,
    data_dir = "data_HALtere"
) {

  print(paste("PUBLICATIONS LIST:", custom_name))

  ## ---- record_publications_in_data -----------------------------------------

  print("EXTRACT PUBLICATIONS")

  publications <- extract_publications(
    custom_name = custom_name,
    query = query,
    data_dir = data_dir
  )

  n1 <- nrow(publications)

  ## ---- record_data_ref_authors_people_and_labs -----------------------------

  print("DATA_REF_AUTHORS, DATA_PEOPLE, DATA_LABS")

  # 1) calculate data_ref_authors
  data_ref_authors <- tidy_ref_authors(
    custom_name,
    data_dir = data_dir
  )

  n2 <- nrow(data_ref_authors)

  # 2) calculate data_words
  data_words <- tidy_words(
    custom_name,
    data_dir = data_dir
  )

  n3 <- nrow(data_words)

  # 3) based on data_ref_authors, group by person
  data_people <- tidy_groups(
    custom_name,
    data_dir = data_dir,
    type = "people"
  )

  n4 <- nrow(data_people)

  # 4) based on data_ref_authors, group by lab
  data_labs <- tidy_groups(
    custom_name,
    data_dir = data_dir,
    type = "labs"
  )

  n5 <- nrow(data_labs)

  ## ---- crossed networks ---------------------------------------------------

  print("CROSSED_PEOPLE,CROSSED_LABS")

  crossed_people <- HALtere::cross(
    custom_name,
    data_dir = data_dir,
    type = "people"
  )

  crossed_labs <- HALtere::cross(
    custom_name,
    data_dir = data_dir,
    type = "labs"
  )

  n6 <- nrow(crossed_people)

  n7 <- nrow(crossed_labs)

  ## ---- text ---------------------------------------------------------------

  print("TEXT")

  text <- tidy_text(
    custom_name,
    data_dir = data_dir,
    column = "text"
  )

  n8 <- nrow(text)

  result <- paste0(
    "For publications ", custom_name, ", \n",
    n1, " publications were collected, corresponding to:\n",
    n2, " references*authors,\n",
    n3, " words metadata,\n",
    n4, " people,\n",
    n5, " labs,\n",
    n6, " links between people,\n",
    n7, " links between labs,\n",
    n8, " words in text."
  )

  cat(
    as.character(Sys.time()),
    file = glue::glue(
      "{data_dir}/{custom_name}/last_modif.txt"
    )
  )

  return(result)
}
