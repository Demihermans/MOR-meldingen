# Laad packages
library(tidyverse)

# Laad Nederlands fastText model voor word embeddings
# Mega traag...
#fasttext <- data.table::fread("clean_data/cc.nl.300.vec", skip = 1, header = FALSE)

nl_vectors <- data.table::fread("clean_data/320/combined-320.txt", skip = 1, header = FALSE)

# Geef de kolommen een naam
colnames(nl_vectors) <- c('woord', paste('dim', 1:320, sep = '_'))

# Functie om de vector van een woord te verkrijgen
get_vector <- function(woord) {
  nl_vectors[which(nl_vectors$woord == woord), -1] |> unlist()
}

# Voorbeeld van het verkrijgen van de vector voor het woord "huis"
#vector_huis <- get_vector("huis")

#nl_vectors$woord == "huis"
