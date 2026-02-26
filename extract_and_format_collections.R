## ----setup-------------------------------------------------------------------------------------------------------------
library(HALtere)
collections=c("BIOEENVIS",
              "EVS_UMR5600",
              "OSR",
              "LEHNA",
              "ECOMIC",
              "OHM-VALLEE_DU_RHONE",
              "LABEX-DRIIHM",
              "PEPR_ONEWATER"
              )

## ----record_publications_in_data---------------------------------------------------------------------------------------
print("LIST OF PUBLICATIONS")
for(collection in collections){
  print(collection)
  datadir=glue::glue("data/{collection}")
  dir.create(datadir,showWarnings = FALSE)
  publications=extract_collection(collection)
  rm(publications)
}

## ----record_data_ref_authors_people_and_labs---------------------------------------------------------------------------
print("DATA_REF_AUTHORS, DATA_PEOPLE, DATA_LABS")
for(collection in collections){
  print(collection)

  # Consider the list of publications already retrieved
  datadir=glue::glue("data/{collection}")
  publications=readRDS(glue::glue("{datadir}/publications.RDS"))

  # 1) calculate data_ref_authors
  data_ref_authors=tidy_ref_authors(publications)
  saveRDS(data_ref_authors,glue::glue("{datadir}/data_ref_authors.RDS"))

  # 2) calculate data_words

  data_ref_authors=readRDS(glue::glue("{datadir}/data_ref_authors.RDS"))
  data_words=tidy_words(data_ref_authors)
  saveRDS(data_words,glue::glue("{datadir}/data_words.RDS"))

  # 3) based on data_ref_authors, group by person
  data_people=tidy_groups(data_ref_authors, type="people")
  saveRDS(data_people,glue::glue("{datadir}/data_people.RDS"))

  # 4) based on data_ref_authors, group by lab
  data_labs=tidy_groups(data_ref_authors, type="labs")
  saveRDS(data_labs,glue::glue("{datadir}/data_labs.RDS"))

  # 5) make space in environment
  rm(data_ref_authors,data_words,data_people,data_labs)
}


## ----------------------------------------------------------------------------------------------------------------------
print("CROSSED_PEOPLE,CROSSED_LABS")
for(collection in collections){
  print(collection)
  datadir=glue::glue("data/{collection}")

  # Retrieve data_ref_authors
  data_ref_authors=readRDS(glue::glue("{datadir}/data_ref_authors.RDS"))

  # Retrieve data_people and cross people
  data_people=readRDS(glue::glue("{datadir}/data_people.RDS"))
  crossed_people=HALtere::cross(data_people,data_ref_authors)
  saveRDS(crossed_people,glue::glue("{datadir}/crossed_people.RDS"))

  # Retrieve data_labs and cross labs
  data_labs=readRDS(glue::glue("{datadir}/data_labs.RDS"))
  crossed_labs=HALtere::cross(data_labs,data_ref_authors)
  saveRDS(crossed_labs,glue::glue("{datadir}/crossed_labs.RDS"))

  rm(data_ref_authors, data_people,data_labs, crossed_people,crossed_labs)

}


## ----------------------------------------------------------------------------------------------------------------------
print("TEXT.RDS")
for(collection in collections){
  print(collection)
  datadir=glue::glue("data/{collection}")


  # Consider the list of publications already retrieved
  publications=readRDS(glue::glue("{datadir}/publications.RDS"))

  # Calculate the table with one row=one word
  text=tidy_text(publications,"text")
  saveRDS(text,
          glue::glue("{datadir}/text.RDS"))

  rm(publications,text)
}

