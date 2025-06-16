# Laad packages
library(tidyverse)

# Laad Nederlands fastText model voor word embeddings
nl_vectors <- data.table::fread("clean_data/cc.nl.300.vec", skip = 1, header = FALSE)

#nl_woorden <- nl_vectors[[1]]

# Geef de kolommen een naam
colnames(nl_vectors) <- c('woord', paste('dim', 1:300, sep = '_'))

# Laat eerste kolom weg en zet om naar matrix met als rijnaam de woorden
#nl_vectors <- nl_vectors[, -1, with = FALSE] |> 
#  as.matrix()

#rownames(nl_vectors) <- nl_woorden

#nl_vectors["huis", ]

# Laad fixi tekst data in en vind de unieke woorden in alle  meldingen
data <- readRDS("clean_data/fixi_tekst_clean.rds")

data_woorden <- unique(data$lemma)

# Kijk welke woorden niet in de word embeddings zitten
print(data |> 
  filter(!lemma %in% nl_vectors$woord) |> 
  count(lemma, sort = TRUE), n = 20)
# Veelal typfouten, maar ook "auto'", overlasten, kinderwag, ongediert, fietspeden

# Bereken woord frequentie per melding
word_freq <- data |> 
  count(doc_id, lemma, sort = TRUE) |> 
  group_by(doc_id)

# Voeg de word embeddings toe met een join op lemma en de row_names van nl_vectors
doc_vectors <- word_freq |> 
  left_join(nl_vectors, by = c("lemma" = "woord")) |> 
  # Vermenigvuldig de frequentie met de embeddings
  mutate(across(.cols = starts_with("dim_"), .fns = function(x){n * x})) |> 
  # Groepeer en tel de embeddings bij elkaar op
  group_by(doc_id) |>
  summarise(across(.cols = where(is.numeric), .fns = sum, na.rm = TRUE)) |> 
  # Deel door n om gewogen gemiddelde te krijgen
  mutate(across(.cols = starts_with("dim_"), .fns = function(x){x / n}))

saveRDS(doc_vectors, "clean_data/fixi_word_embeddings.rds")
