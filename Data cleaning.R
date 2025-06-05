
## preprocessing data ##

library(tidyverse)
library(stringr)
library(spacyr)
library(dplyr)
library(udpipe)
library(tm)


# Opschonen data


# Verwijderen van persoonsnamen en adressen

  # # 1) Download en laad het Nederlandse Udpipe-model (éénmalig)
  # model <- udpipe_download_model(language = "dutch")
  # ud    <- udpipe_load_model(model$file_model)
  # 
  # 
  # # 2) Annotatie op de ORIGINELE Omschrijving
  # anno   <- udpipe_annotate(ud,
  #                           x      = as.character(df_MORmeldingen$Omschrijving),
  #                           doc_id = as.character(seq_len(nrow(df_MORmeldingen))))
  # anno_df <- as.data.frame(anno)
  # 
  # # 3) Verzamel per document alle proper nouns (PROPN), in lowercase
  # propn_per_doc <- anno_df %>%
  #   filter(upos == "PROPN") %>%
  #   mutate(token = tolower(token)) %>%
  #   group_by(doc_id) %>%
  #   summarise(to_remove = list(unique(token)), .groups = "drop")
  # 
  # # 4) Helper om tokens uit een tekst te verwijderen
  # remove_tokens <- function(text, tokens){
  #   for(t in tokens) {
  #     text <- str_remove_all(text, paste0("\\b", t, "\\b"))
  #   }
  #   text
  # }
  # 
  # # 5) Samenvoegen en opschonen
  # df_clean <- df_MORmeldingen %>%
  #   mutate(doc_id = as.character(seq_len(n()))) %>%
  #   left_join(propn_per_doc, by = "doc_id") %>%
  #   rowwise() %>%
  #   mutate(
  #     # a) lowercase + grove opschoning
  #     tmp = tolower(Omschrijving),
  #     tmp = str_remove_all(tmp, "[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]{2,}"),  # e-mail
  #     tmp = str_remove_all(tmp, "\\b\\d{4}\\s?[a-z]{2}\\b"),                            # postcode
  #     tmp = str_remove_all(tmp, "\\b[[:alpha:]]+[[:space:]]+\\d+[[:alpha:]]?\\b"),       # straat + huisnr
  #     tmp = str_replace_all(tmp, "[^[:alpha:][:space:]]", " "),                        # overig niet-letter
  #     tmp = str_squish(tmp),
  #     
  #     # b) proper nouns verwijderen
  #     Omschrijving_clean = remove_tokens(tmp, to_remove %||% character(0)),
  #     
  #     # c) final cleanup
  #     Omschrijving_clean = str_squish(Omschrijving_clean)
  #   ) %>%
  #   ungroup() %>%
  #   select(-doc_id, -to_remove, -tmp)
  

  
  
  #########
  
  ##NA's uit omschrijving halen voor verdere analyse:
  df_clean_MOR <- df_MORmeldingen %>%
    mutate(Omschrijving = as.character(Omschrijving))%>%
    mutate(Omschrijving = ifelse(is.na(Omschrijving), "", Omschrijving))
  
  df_clean_MOR <- df_clean_MOR %>%
    mutate(Omschrijving = ifelse(is.na(Omschrijving) | str_trim(Omschrijving)=="",
                                 "<empty>", Omschrijving))

  #udpipe backend voor tokenization, part of speech tagging, lemmatization, dependency parsing
  cleanNLP::cnlp_init_udpipe("dutch")
  corpus <- cleanNLP::cnlp_annotate(df_clean_MOR$Omschrijving)
  View(corpus$token) 
  
  # Vind de parent token voor elke token in de tekst door te joinen op tid_source en tid
  # Omdat tid een nummer is wat weer bij 0 begint bij een volgende zin moet je joinen op drie kolommen
  dependencies <- corpus$token %>%
    select(tid_source, doc_id, sid, token) %>%
    left_join(corpus$token,
              by = c("tid_source" = "tid", "doc_id" = "doc_id", "sid" = "sid"),
              suffix = c(".child", ".parent"))
  
  View(dependencies)
  
  ##hier kan je punct namen etc. eruit halen.
  
  corpus$token %>%
    filter(upos == "PROPN") %>%      # filter op PROPN
    head(20)                           # toon de eerste 20 voorbeelden

  #De volgende upos wil eruit halen:
  # ADP (voorkombindwoorden zoals voorzetsels), PUNCT (interpunctietekens), NUM (cijfers), SYM (tekens zoals € %), 
  # AUX (hulpwerkwoorden zoals zijn, hebben), PRON (voornaamwoorden zoals ik, jij, hij), DET (lidwoorden en aanwijzende/ bezittelijke voornaamwoorden zoals de, dit)
  # CCONJ (nevenschikkende voegwoorden zoals en, maar) 
  # X overig weet ik nog niet.
  
  # Verwijderen van ongewenste upos
  df_clean_MOR_gefilterd <- corpus$token %>%
    filter(!upos %in% c("ADP", "PUNCT", "NUM", "SYM", "AUX", "PRON", "DET", "CCONJ", "X"))
  
  #hiermee zijn ook al veel voorkomende stopwoorden verwijderd. 
  #voor de zekerheid doe ik ook nog een anti-join met nederlandse stopwoordenlijst
  
 #stopwoordenlijsten samenvoegen
  
  #verschillende stopwoordenlijsten
  tm_stop <- tibble(word = tm::stopwords("dutch")) #101 woorden
  stopwords_nl <- tibble(word = lsa::stopwords_nl) #260 woorden
  
   # Combineer en haal alleen unieke woorden
  combined_stop <- bind_rows(tm_stop, stopwords_nl) %>%
    distinct(word) %>%
    arrange(word) #289 woorden
  
  
  #Hoeveel stopwoorden zitten er nog in gecleande data?
  # Aantal rijen in corpus$token
  totaal_tokens <- nrow(df_clean_MOR_gefilterd)
  
  # Tel alle tokens (op lemma-niveau) die in de stopwoordenlijst staan
  stop_tokens <- df_clean_MOR_gefilterd %>%
    # gebruik lemma (of token) afhankelijk van je voorkeur
    filter(lemma %in% combined_stop$word)
  
  aantal_stop_tokens      <- nrow(stop_tokens)
  aantal_verschillende   <- stop_tokens %>% distinct(lemma) %>% nrow()
  
  # 227 verschillende stopwoorden uit de combined_stop lijst (uit in totaal 256.273 rijen) in dataset. 
  
  #eigen stopwoordenlijst maken
  eigen_stop <- tibble(
    word = c("NIL", "<empty>", "staan","liggen", "komen", "zien", "maken", "zitten", "willen", "gaan", 
             "graag","melding", "gemeente", "Roosendaal")
    )
  
  combined_stop <- bind_rows(combined_stop, eigen_stop) %>%
    distinct(word) %>%
    arrange(word) #296 woorden
  
  # Deze stopwoorden verwijderen we nu uit gecleande dataset:
  
  df_clean_MOR_gefilterd_stopwoorden <- df_clean_MOR_gefilterd %>%
    anti_join(combined_stop, by = c("lemma" = "word"))
  
  
  ##Rijnummer toevoegen aan de dataset zodat dit makkelijker te koppelen is met doc_id
  df_clean_MOR <- df_clean_MOR %>%
    mutate(doc_id = row_number())