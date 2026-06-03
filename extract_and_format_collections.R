## ----setup-------------------------------------------------------------------------------------------------------------

library(HALtere)

cat(
  "This directory is a HALtere data directory",
  file = "data_HALtere/HALtere_data_directory_README.txt"
)

HALtere_directories <- dplyr::bind_rows(

  # prepare_HALtere_directory(
  #   query = 'collCode_s:"BIOEENVIS"'
  # ),

  # prepare_HALtere_directory(
  #   query = 'collCode_s:"EVS_UMR5600"'
  # ),

  # prepare_HALtere_directory(
  #   query = 'collCode_s:"OSR"'
  # ),

  # prepare_HALtere_directory(
  #   query = 'collCode_s:"LEHNA"'
  # ),

  # prepare_HALtere_directory(
  #   query = 'collCode_s:"ECOMIC"'
  # ),

  # prepare_HALtere_directory(
  #   query = 'collCode_s:"OHM-VALLEE_DU_RHONE"'
  # ),

  # prepare_HALtere_directory(
  #   query = 'collCode_s:"LABEX-DRIIHM"'
  # ),

  # prepare_HALtere_directory(
  #   query = 'collCode_s:"PEPR_ONEWATER"'
  # ),
#
#   prepare_HALtere_directory(
#     custom_name = "Lise Vaudor",
#     query = 'authIdHal_s:"lise-vaudor"'
#   ),
#
#   prepare_HALtere_directory(
#     custom_name = "Barbara Belletti",
#     query = 'authIdPerson_i:"183708"'
#   ),
#
#   prepare_HALtere_directory(
#     custom_name = "GloUrb",
#     query = 'anrProjectReference_s:"ANR-22-CE03-0005"'
#   ),
#
#   prepare_HALtere_directory(custom_name="Rencontres_R",
#                             query = 'title_autocomplete:"Rencontres R"'
#                             ),
  prepare_HALtere_directory(custom_name="Packages_R",
                            query = 'title_autocomplete:"R package"'
  )
)

for (i in seq_len(nrow(HALtere_directories))) {
  custom_name <- HALtere_directories$custom_name[i]
  query <- HALtere_directories$query[i]
  prepare_all_data(
    custom_name = custom_name,
    query = query,
    data_dir = "data_HALtere"
  )

}
