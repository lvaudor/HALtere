## ----setup-------------------------------------------------------------------------------------------------------------
library(HALtere)
df_identifiers=dplyr::bind_rows(
  tibble::tibble(identifier="BIOEENVIS",
                 hal_variable="collCode_s"),
  tibble::tibble(identifier="EVS_UMR5600",
                 hal_variable="collCode_s"),
  tibble::tibble(identifier="OSR",
                 hal_variable="collCode_s"),
  tibble::tibble(identifier="LEHNA",
                 hal_variable="collCode_s"),
  tibble::tibble(identifier="ECOMIC",
                 hal_variable="collCode_s"),
  tibble::tibble(identifier="OHM-VALLEE_DU_RHONE",
                 hal_variable="collCode_s"),
  tibble::tibble(identifier="LABEX-DRIIHM",
                 hal_variable="collCode_s"),
  tibble::tibble(identifier="PEPR_ONEWATER",
                 hal_variable="collCode_s"),
  tibble::tibble(identifier="lise-vaudor",
                 hal_variable="authIdHal_s"),
  tibble::tibble(identifier="183708",
                 hal_variable="authIdPerson_i"),
  tibble::tibble(identifier="ANR-22-CE03-0005",
                  hal_variable="anrProjectReference_s")
)
for(i in 1:nrow(df_identifiers)){
  identifier=df_identifiers$identifier[i]
  hal_variable=df_identifiers$hal_variable[i]
  prepare_all_data(identifier, hal_variable)
}

