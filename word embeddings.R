# Laad packages
library(tidyverse)

# Laad Nederlands fastText model voor word embeddings
fasttext <- data.table::fread("clean_data/cc.nl.300.vec", skip = 1, header = FALSE)

# Geef de kolommen een naam
colnames(fasttext) <- c('woord', paste('dim', 1:300, sep = '_'))

# Functie om de vector van een woord te verkrijgen
get_vector <- function(woord) {
  fasttext[which(fasttext$woord == woord), -1] |> unlist()
}

# Voorbeeld van het verkrijgen van de vector voor het woord "huis"
vector_huis <- get_vector("huis")
