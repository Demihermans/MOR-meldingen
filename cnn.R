library(keras)
library(tensorflow)
library(tidyverse)

# Laad data verwerkt met word embeddings in en de metadata voor de categorieën
doc_vectors <- readRDS("clean_data/fixi_word_embeddings.rds")
metadata <- readRDS("clean_data/fixi_metadata_clean.rds")

# Voeg categorieën toe aan de document vectoren
doc_vectors <- doc_vectors |> 
  left_join(metadata |> select(id, categoryName), by = join_by(doc_id == id))

# Verwijder de doc_id en n kolom
doc_vectors <- doc_vectors |> 
  select(-doc_id, -n) 

# Aantal dimensies van de word embeddings
embeddings_dim <- 300

# Aantal gewenste output units van model (aantal categorieën)
categories <- length(unique(metadata$categoryName))

# Bereid data voor om in "simpel" model te stoppen
set.seed(42)

# Alle dimensies
x_data <- doc_vectors |>
  select(-categoryName) |>
  as.matrix()

# Het label, omgezet naar categorische numeriek variabelen (one-hot encoded)
y_data <- to_categorical(as.integer(as.factor(doc_vectors$categoryName)) - 1)

# Lijstje om de categorieën te kunnen gebruiken
categories_list <- levels(as.factor(doc_vectors$categoryName))

indices <- sample(1:nrow(x_data), size = 0.8 * nrow(x_data))

x_train <- x_data[indices, ]
y_train <- y_data[indices, ]

x_test <- x_data[-indices, ]
y_test <- y_data[-indices, ]


# Definieer "simpel" deep learning model
model <- keras_model_sequential() |>
  # Input shape gelijk aan aantal dimensie kolommen
  layer_dense(units = 128, activation = 'relu', input_shape = embeddings_dim) |>
  # Dropout om overfitting te voorkomen
  layer_dropout(rate = 0.3) |>
  layer_dense(units = 64, activation = 'relu') |>
  layer_dropout(rate = 0.3) |>
  # Laatste laag met aantal units gelijk aan te voorspellen categorieën en softmax voor classificatie
  layer_dense(units = categories, activation = 'softmax')

# # Bereid data voor om in convolutional model te stoppen
# x_data <- doc_vectors |> 
#   select(-categoryName) |> 
#   as.matrix() |> 
#   array_reshape(dim = c(nrow(doc_vectors), embeddings_dim, 1))
# 
# # Het label, omgezet naar categorische numeriek variabelen (one-hot encoded)
# y_data <- to_categorical(as.integer(as.factor(doc_vectors$categoryName)) - 1)
# 
# # Lijstje om de categorieën te kunnen gebruiken
# categories_list <- levels(as.factor(doc_vectors$categoryName))
# 
# indices <- sample(1:nrow(x_data), size = 0.8 * nrow(x_data))
# 
# x_train <- x_data[indices, ,]
# y_train <- y_data[indices, ]
# 
# x_test <- x_data[-indices, ,]
# y_test <- y_data[-indices, ]
# 
# # Zwaarder model
# model <- keras_model_sequential() |> 
#   layer_conv_1d(filters = 128, kernel_size = 5, activation = 'relu', input_shape = c(300,1)) |> 
#   layer_max_pooling_1d(pool_size = 5) |> 
#   layer_conv_1d(filters = 64, kernel_size = 5, activation = 'relu') |> 
#   layer_max_pooling_1d(pool_size = 5) |> 
#   layer_flatten()  |> 
#   layer_dense(units = 128, activation = 'relu') |> 
#   layer_dropout(rate = 0.3) |> 
#   layer_dense(units = categories, activation = 'softmax')

# Compileer het model met een optimizer, verliesfunctie en welke metrics
model |>  compile(
  optimizer = 'adam',
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

# Fit het model op de training data
model |> fit(
  x_train, y_train,
  epochs = 20,
  batch_size = 64,
  validation_split = 0.2
)

# Evalueer op test set
model |> evaluate(x_test, y_test)

# Maak voorspellingen op de test set
predictions <- model |> predict(x_test)

# Zet voorspellingen om naar categorieën
voorspelde_cat <- apply(predictions, 1, which.max) - 1

categories_list[voorspelde_cat + 1]
categories_list[y_test |> apply(1, which.max)]

# Vergelijk voorspelde categorieën met de werkelijke categorieën
# conf_matrix <- table(
#   Predicted = as.factor(voorspelde_cat),
#   Actual = as.factor(apply(y_test, 1, which.max) - 1)
# )
conf_matrix <- table(
  Predicted = categories_list[voorspelde_cat + 1],
  Actual = categories_list[y_test |> apply(1, which.max)]
)

# Print de confusion matrix
print(conf_matrix)
print(categories_list)
