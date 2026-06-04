library(tidyverse)

lexicon_en=readr::read_delim("data-raw/lexique_en.txt",
                    "\t", escape_double = FALSE, col_names = FALSE,
                    trim_ws = TRUE) %>%
    dplyr::select(1:3)
colnames(lexicon_en)=c("word","lemma","type")


lexicon_en_missing=readr::read_csv("data-raw/missing_words_lexicon_en.csv") %>%
  mutate(type="unspecified")
lexicon_en=lexicon_en %>%
  bind_rows(lexicon_en_missing) %>%
  arrange(word,lemma)
usethis::use_data(lexicon_en, overwrite=TRUE)
