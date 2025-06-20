# Supervised classificatie van MOR-meldingen met BERTje-embeddings in R

# 1. Installeer en laad benodigde packages
install.packages(c("text", "textEmbedModels", "caret", "readr", "dplyr"))
library(text)
library(textEmbedModels)
library(caret)
library(readr)
library(dplyr)

#overschakelen naar Conda omgeving waar transformers en torch geinstalleerd zijn, tokenizen en per token vector van 768 dimensies teruggeven met feature-extraction
use_condaenv("bertje-env", required = TRUE)
transformers <- import("transformers")
tokenizer    <- transformers$AutoTokenizer$from_pretrained("GroNLP/bert-base-dutch-cased")
embedder     <- transformers$pipeline(
  task      = "feature-extraction",
  model     = "GroNLP/bert-base-dutch-cased",
  tokenizer = tokenizer
)

# 2. Lees gelabelde dataset in 
data <- df_clean_MOR %>% 
  filter(!is.na(Categorie) & Categorie != "") %>%
  sample_n(size = 1000)

# 3. factor-levels 'veilig' zonder spaties of leestekens maken anders error
label_levels <- unique(data$Categorie)
safe_levels  <- make.names(label_levels)
data$Categorie  <- factor(data$Categorie, levels = label_levels, labels = safe_levels)

# 4. Train/test split (80/20)
set.seed(123)
train_idx  <- createDataPartition(data$Categorie, p = 0.8, list = FALSE)
train_data <- data[train_idx, ]
test_data  <- data[-train_idx, ]

# 5. Functie om embeddings te genereren (mean-pooling over tokens)
get_embedding <- function(text) {
  emb_list <- embedder(
    text,
    truncation  = TRUE,
    max_length  = as.integer(512) #lange meldingen worden afgeknipt anders krijg je error
  )[[1]]
  emb_r <- py_to_r(emb_list) #zet python uitvoer om naar R matrix/vectror
  if (is.list(emb_r) && !is.numeric(emb_r)) { #mean pooling; per kolom (gelegenheidsdimensie) het gemiddelde over alle tokens zodat elke medling 1 vector oplevert.
    mat <- do.call(rbind, emb_r)
  } else {
    mat <- emb_r
  }
  if (is.null(dim(mat))) {
    return(as.numeric(mat))
  } else {
    return(colMeans(mat))
  }
}

# 6. Bereken embeddings voor train en test met purrr en bind tot matrix
train_embeddings <- purrr::map(train_data$Omschrijving, get_embedding)
train_mat        <- do.call(rbind, train_embeddings)
test_embeddings  <- purrr::map(test_data$Omschrijving, get_embedding)
test_mat         <- do.call(rbind, test_embeddings)

# 7. Combineer embeddings met labels tot dataframes
train_df <- as.data.frame(train_mat) %>% mutate(Label = train_data$Categorie)
test_df  <- as.data.frame(test_mat)  %>% mutate(Label = test_data$Categorie)


# 8. Train een Random Forest classifier middels caret
set.seed(123)
ctrl    <- trainControl(method = "cv", number = 5, classProbs = TRUE)
rf_model <- train(
  Label ~ ., data = train_df,
  method    = "ranger",       # snelle Random Forest via ranger
  trControl = ctrl
)

# 9. Evalueer op de testset
eval_pred <- predict(rf_model, test_df)
# Forceer dezelfde levels als in test_df$Label
eval_pred <- factor(eval_pred, levels = levels(test_df$Label))
# Bereken confusiematrix
eqm <- confusionMatrix(data = eval_pred, reference = test_df$Label)
print(eqm)

# 10. Voorspellen op nieuwe meldingen
new_data <- read_csv("mor_unlabeled.csv", col_types = cols(Omschrijving = col_character()))
new_mat  <- t(sapply(new_data$Omschrijving, get_embedding))
new_df   <- as.data.frame(new_mat)
new_pred <- predict(rf_model, new_df)
new_prob <- predict(rf_model, new_df, type = "prob")
results  <- bind_cols(new_data, tibble(Predicted = new_pred, new_prob))

# 11. Opslaan en inspectie
write_csv(results, "mor_predictions.csv")
print(head(results))