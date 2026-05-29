#' Get all HALtere data
#' @param collection a collection name
#' @param hal_variable the HAL variable used to retrieve items. Can be collCode_s (for collections), authIdHal_s (for person)
#' @return the characteristics of all datasets generated for the collection. Can be "collection", "person", "lab".
#' @export
#' @examples
#'prepare_all_data("lise-vaudor",hal_variable="person")
prepare_all_data=function(collection, hal_variable="collection"){
    print(paste("COLLECTION:", collection))
    ## ----record_publications_in_data---------------------------------------------------------------------------------------
    print("LIST OF PUBLICATIONS")
    publications=extract_collection(collection, data_dir="data", hal_variable=hal_variable)
    n1=nrow(publications)
    ## ----record_data_ref_authors_people_and_labs---------------------------------------------------------------------------
    print("DATA_REF_AUTHORS, DATA_PEOPLE, DATA_LABS")
    # 1) calculate data_ref_authors
    data_ref_authors=tidy_ref_authors(collection, data_dir="data")
    n2=nrow(data_ref_authors)
    # 2) calculate data_words
    data_words=tidy_words(collection, data_dir="data")
    n3=nrow(data_words)
    # 3) based on data_ref_authors, group by person
    data_people=tidy_groups(collection, data_dir="data", type="people")
    n4=nrow(data_people)
    # 4) based on data_ref_authors, group by lab
    data_labs=tidy_groups(collection, data_dir="data", type="labs")
    n4=nrow(data_labs)
    ## ----------------------------------------------------------------------------------------------------------------------
    print("CROSSED_PEOPLE,CROSSED_LABS")
    crossed_people=HALtere::cross(collection, data_dir="data", type="people")
    crossed_labs=HALtere::cross(collection, data_dir="data", type="labs")
    n5=nrow(crossed_people)
    n6=nrow(crossed_labs)
    ## ----------------------------------------------------------------------------------------------------------------------
    print("TEXT")
    # Calculate the table with one row=one word
    text=tidy_text(collection, column="text")
    n7=nrow(text)
    result=paste0(
      "For collection ", collection, ", \n",
      n1, "publications were collected, corresponding to :\n",
      n2, "references*authors,\n",
      n3, "people,\n",
      n4, "labs,\n",
      n5, "links between people,\n",
      n6, "links between labs,\n",
      n7, "words."
    )
    return(result)
}
