# Laad packages
library(tidyverse)
library(tidymodels)
library(tidytext)

# Laad data (tokenized dataset van de meldingen) en metadata (kenmerken van meldingen)
fixi_data <- readRDS("clean_data/fixi_tekst_clean.rds")
fixi_metadata <- readRDS("clean_data/fixi_metadata_clean.rds")

# Voeg categorie toe aan fixi_data
fixi_data <- fixi_data |> 
  left_join(fixi_metadata |> select(id, categoryName), by = join_by(doc_id == id))

# Laat categorie OVerlast en handhaving weg -> te generiek voor dit model
fixi_data <- fixi_data |> 
  filter(categoryName != "Overlast en handhaving")

# Tf-idf score berekenen van de meest voorkomende woorden
top_words <- fixi_data |> 
  count(lemma, sort = TRUE) |> 
  slice_max(n, n = 750)

# 10 meest voorkomende woorden per categorie
top_words_per_cat <- fixi_data |> 
  count(categoryName, lemma, sort = TRUE) |>
  group_by(categoryName) |> 
  slice_max(n, n = 10) |> 
  ungroup()

word_freq <- fixi_data |> 
  count(doc_id, lemma, sort = TRUE) %>%
  bind_tf_idf(term = lemma, document = doc_id, n = n) |> 
  semi_join(top_words, by = "lemma")

doc_vectors <- word_freq |> 
  pivot_wider(id_cols = doc_id, names_from = lemma, values_from = tf_idf, values_fill = 0)

# Voeg categorieën toe aan de document vectoren
doc_vectors <- doc_vectors |> 
  left_join(fixi_metadata |> select(id, categoryName), by = join_by(doc_id == id))

# Maak een factor van de categoryName kolom
doc_vectors <- doc_vectors |> 
  mutate(categoryName = as.factor(categoryName))

doc_vectors |> 
  count(categoryName)

# Misschien nog SMOTE om verdeling rechter te trekken??


# Splits data in training en test set
set.seed(42)

# Voor eerste poging maar 10% van de data gebruiken
doc_vectors <- doc_vectors |> 
  sample_frac(0.1)

data_split <- initial_split(doc_vectors, prop = 0.8, strata = categoryName)

train_data <- training(data_split)
test_data <- testing(data_split)

# Maak een recept (workflow)
recipe_spec <- recipe(categoryName ~ ., data = train_data) |> 
  # Doc id wordt niet gebruikt in de model training
  update_role(doc_id, new_role = "ID") |> 
  # Verwijder kolommen met variantie = 0
  step_zv(all_predictors()) |> 
  # Normaliseer de numerieke waarden
  step_normalize(all_predictors())

# Bak het recept
baked_train <- recipe_spec |> 
  prep() |> 
  bake(new_data = NULL)

baked_test <- recipe_spec |> 
  prep() |> 
  bake(new_data = test_data)

# SVM model initialiseren
svm_spec <- svm_rbf(
  mode = "classification",
  cost = 10, # tune(),
  rbf_sigma = 0.01, # tune()
) |> 
  set_engine("kernlab")

# Pas het recept toe middels een workflow
fixi_wf <- workflow() |> 
  add_recipe(recipe_spec) |> 
  add_model(svm_spec)

# Maak een grid van waarden om mee te trainen
grid <- expand.grid(
  cost = c(1, 10, 100),
  rbf_sigma = c(0.01, 0.1, 1)
)

# Train het model met de grid search
model <- tune_grid(
  fixi_wf,
  resamples = vfold_cv(train_data, v = 5),
  grid = grid
)

best_params <- select_best(model, metric = "accuracy")

# Pas de beste parameters toe op het model
fixi_wf <- finalize_workflow(
  fixi_wf,
  best_params
)

# Train het model
fixi_fit <- fixi_wf |> 
  fit(data = baked_train)

# Maak voorspellingen op de test set
fixi_pred <- predict(fixi_fit, test_data)

# Vergelijk voorspelling met daadwerkelijke categorie
results <- bind_cols(test_data, fixi_pred) |> 
  rename(predicted_category = .pred_class)

# Bereken de nauwkeurigheid
accuracy <- results |> 
  metrics(truth = categoryName, estimate = predicted_category) |> 
  filter(.metric == "accuracy") |> 
  pull(.estimate)

results |> 
  select(doc_id, categoryName, predicted_category)
