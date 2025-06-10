# Laad packages
library(tidyverse)
library(tidymodels)
library(tidytext)

# Laad data (tokenized dataset van de mor meldingen)
# mor_data <- readRDS(...)
fixi_data <- readRDS("clean_data/fixi_clean.rds")
fixi_metadata <- readRDS("clean_data/fixi_metadata_clean.rds")

# Tf-idf score berekenen van de meest voorkomende woorden
top_words <- fixi_data |> 
  count(lemma, sort = TRUE) |> 
  slice_max(n, n = 500)

word_freq <- fixi_data |> 
  count(doc_id, lemma, sort = TRUE) %>%
  bind_tf_idf(term = lemma, document = doc_id, n = n) |> 
  semi_join(top_words, by = "lemma")

doc_vectors <- word_freq |> 
  pivot_wider(id_cols = doc_id, names_from = lemma, values_from = tf_idf, values_fill = 0)

# Voeg categorieën toe aan de document vectoren
doc_vectors <- doc_vectors |> 
  left_join(fixi_metadata |> select(doc_id, categoryName), by = "doc_id")

# Maak een factor van de categoryName kolom
doc_vectors <- doc_vectors |> 
  mutate(categoryName = as.factor(categoryName))

# Splits data in training en test set
set.seed(42)
data_split <- initial_split(doc_vectors, prop = 0.8, strata = categoryName)

train_data <- training(data_split)
test_data <- testing(data_split)

# Train een logistische regressie model
logreg <- glm(categoryName ~ ., data = train_data, family = "binomial")

# Maak voorspellingen met het model
logreg_pred <- predict(logreg, newdata = test_data)