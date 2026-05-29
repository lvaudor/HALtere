#' Retrieve a human-readable HAL label from an identifier
#'
#' @param identifier identifier value
#' @param identifier_type HAL field used as identifier
#'
#' @return character vector
#' @export

get_hal_labels <- function(identifier,
                           identifier_type) {
browser()
# Endpoint et champs selon le type d'identifiant
config <- switch(

  identifier_type,

  # -------------------------
  # Auteurs
  # -------------------------
  authIdHal_s = list(
    endpoint = "https://api.archives-ouvertes.fr/ref/author/",
    q = glue::glue('{identifier_type}:"{identifier}"'),
    fl = "fullName_s"
  ),

  authId_i = list(
    endpoint = "https://api.archives-ouvertes.fr/ref/author/",
    q = glue::glue('{identifier_type}:"{identifier}"'),
    fl = "fullName_s"
  ),

  # -------------------------
  # Structures
  # -------------------------

  structIdHal_s = list(
    endpoint = "https://api.archives-ouvertes.fr/ref/structure/",
    q = glue::glue('{identifier_type}:"{identifier}"'),
    fl = "name_s"
  ),

  structAcronym_s = list(
    endpoint = "https://api.archives-ouvertes.fr/ref/structure/",
    q = glue::glue('{identifier_type}:"{identifier}"'),
    fl = "name_s"
  ),

  structId_i = list(
    endpoint = "https://api.archives-ouvertes.fr/ref/structure/",
    q = glue::glue('{identifier_type}:"{identifier}"'),
    fl = "name_s"
  ),

  # -------------------------
  # Collections
  # -------------------------

  collCode_s = list(
    endpoint = "https://api.archives-ouvertes.fr/ref/collection/",
    q = glue::glue('{identifier_type}:"{identifier}"'),
    fl = "name_s"
  ),

  # -------------------------
  # Projets ANR
  # -> pas de vrai endpoint ref
  # -------------------------

  anrProjectReference_s = list(
    endpoint = "https://api.archives-ouvertes.fr/search/",
    q = glue::glue('{identifier_type}:"{identifier}"'),
    fl = "anrProjectAcronym_s"
  ),

  stop(
    glue::glue(
      "Unsupported identifier_type: {identifier_type}"
    )
  )
)

params <- list(
  q = config$q,
  rows = 1,
  wt = "json",
  fl = config$fl
)

res <- httr::GET(
  config$endpoint,
  query = params
) |>
  httr::content(
    as = "text",
    encoding = "UTF-8"
  ) |>
  jsonlite::fromJSON()

docs <- res$response$docs

if (length(docs) == 0) {
  return(NA_character_)
}

out <- docs[[config$fl]]

if (is.list(out)) {
  out <- unlist(out)
}

return(out[1])
}
