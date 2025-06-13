
library(text2vec)
library(data.table)
library(Matrix)
library(tibble)
library(tidytext)
library(stats)
library(caret)
library(ranger)
library(xgboost)
library(factoextra)

######### Word embeddings ####################

#Inladen van pre-trained FastText-embeddings (300 dimensies)
#  pak uit (houd de .gz erin staan)
install.packages("R.utils")
library(R.utils)
gunzip("cc.nl.300.vec.gz", remove = FALSE)  

# laad in als dataframe met in kolom 1 een woord en kolom 2-301 de 300 dimensies
ft <- fread(
  "cc.nl.300.vec", 
  header = FALSE, 
  encoding = "UTF-8",
  quote = "",
  data.table = FALSE
)

# splits in woorden + vectoren
words <- ft[[1]] #alle nederlandse woorden
mat   <- as.matrix(ft[, -1]) #numerieke vector 
rownames(mat) <- words

# Nu is 'mat' een matrix [vocab_size × 300] met NL-embeddings

#testen met een woord ('Parkeren)
query <- mat["parkeren", , drop = FALSE]
sims  <- sim2(mat, query, method = "cosine", norm = "l2") #sim2(0 berekent de cosine-similarity tussen alle rijen mat en de query vector
head(sort(sims[,1], decreasing = TRUE), 10) #welke woorden sterkst semantisch verwant zijn aan parkeren

###### Document embeddings maken om in clusteranalyse (supervised modellen) te gebruiken

#sparse dtm

dtm_sparse <- df_clean_MOR_gefilterd_stopwoorden %>%
  count(doc_id, lemma) %>%            # levert kolommen: doc_id, lemma, n
  cast_sparse(doc_id, lemma, n)       # positional args: document, term, value, alleen niet nullen worden opgeslagen.
#hoe vaak komt lemma in doc_id voor

# Haal alle termen (lemmata) uit je DTM op
terms_dtm <- colnames(dtm_sparse)

# Vind welke daarvan wél in je embedding-matrix mat zitten
terms_common <- intersect(terms_dtm, rownames(mat))

# welke missen (kun je achteraf toevoegen of negeren)
missing_terms <- setdiff(terms_dtm, terms_common)
length(missing_terms)      # hoeveel
head(missing_terms)        # welke

#Maak een “gecleanede” DTM met alleen de kolommen waarvoor je embeddings hebt
dtm_clean <- dtm_sparse[, terms_common]

# Subset je embedding-matrix op exact dezelfde termen en in dezelfde volgorde
mat_sub <- mat[terms_common, , drop = FALSE]

# PARANOIA-CHECK: aantallen moeten matchen
stopifnot(ncol(dtm_clean) == nrow(mat_sub))

# Vermenigvuldig: [#docs × #terms_common] %*% [#terms_common × 300]
doc_embeddings_mat <- dtm_clean %*% mat_sub

# Zet het resultaat om in een tibble met doc_id en embedding_1…embedding_300
df_doc_embeddings <- as.matrix(doc_embeddings_mat) %>%
  as_tibble() %>%
  mutate(doc_id = as.integer(rownames(doc_embeddings_mat))) %>%
  relocate(doc_id)

df_doc_embeddings

# Resultaat is een regel per melding met 300 embedding-features 

##################################
#Document clustering met k-means

# 1) Kies een k aan de hand van de “elbow‐method”
fviz_nbclust(
  df_doc_embeddings %>% select(-doc_id),
  kmeans,
  method = "wss"
) +
  labs(subtitle = "Elbow‐plot: binnen‐groep variantie vs. k")

# Kies in de plot het knikpunt, stel k = 6
k <- 6

# 2) Run k‐means
set.seed(42)
km_res <- kmeans(
  df_doc_embeddings %>% select(-doc_id),
  centers = k,
  nstart  = 25
)

# 3) Voeg cluster‐labels toe aan je embeddings‐tabel
df_clustered <- df_doc_embeddings %>%
  mutate(cluster = factor(km_res$cluster))

# 4) Bekijk cluster‐grootte en steekproef meldingen per cluster
df_clustered %>%
  count(cluster) %>%
  arrange(desc(n))

df_clustered %>%
  left_join(df_clean_MOR, by = "doc_id") %>%
  group_by(cluster) %>%
  slice_head(n = 5) %>%
  select(cluster, Omschrijving)

# 5) (Optioneel) Visualiseer in 2D met UMAP
library(uwot)
umap_2d <- umap(
  df_clustered %>% select(starts_with("V")),
  n_neighbors = 15,
  min_dist    = 0.1
)

plot_df <- df_clustered %>%
  transmute(
    doc_id,
    cluster,
    U1 = umap_2d[,1],
    U2 = umap_2d[,2]
  )

ggplot(plot_df, aes(U1, U2, color = cluster)) +
  geom_point(alpha = 0.5) +
  labs(title = "UMAP van document‐embeddings", x = "UMAP1", y = "UMAP2")

## welke meldingen welke clusters
df_clustered %>% 
  filter(cluster == 4) %>% 
  slice_head(n = 5) %>% 
  left_join(df_clean_MOR, by="doc_id") %>% 
  pull(Omschrijving)

###########################
# --- 3) Supervised model training--------

# Data Klaarmaken
# df_labels uit je gelabelde df_clean_MOR
df_labels <- df_clean_MOR %>% select(doc_id, Categorie)

# join embeddings met labels en lege labels eruit filteren
df_data <- df_doc_embeddings %>%
  inner_join(df_labels, by = "doc_id") %>%
  filter(!is.na(Categorie)) %>%
  mutate(
    Categorie_clean = make.names(Categorie),
    Categorie_clean = factor(
      Categorie_clean,
      levels = unique(make.names(Categorie))
    )
  )

# train en test dataset splitsen
set.seed(123)
train_idx <- createDataPartition(df_data$Categorie_clean, p = 0.8, list = FALSE)
train      <- df_data[train_idx, ]
test       <- df_data[-train_idx, ]


# features zijn alle embedding-kolommen
X_train <- train %>% select(-doc_id, -Categorie, -Categorie_clean) %>% as.matrix()
X_test  <- test  %>% select(-doc_id, -Categorie, -Categorie_clean) %>% as.matrix()

# zorg dat beide factor-levels hetzelfde zijn
 levels_cat <- unique(df_train$Categorie)
 y_train    <- as.numeric(factor(train$Categorie, levels = levels_cat)) - 1 #numeriek
 y_test     <- as.numeric(factor(test$Categorie,  levels = levels_cat)) - 1 # numeriek

y_train_fac <- factor(train$Categorie, levels = levels_cat) #factor
y_test_fac  <- factor(train$Categorie, levels = levels_cat) #factor

y_train_fac <- train$Categorie_clean
y_test_fac  <- test$Categorie_clean

# DMatrixen maken voor XGBoost
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test,  label = y_test)

# caret traincontrol instellen voor cross-validatie
ctrl <- trainControl(
  method      = "cv",      # 5-fold CV
  number      = 5,
  classProbs  = TRUE,      # indien je later AUC wilt berekenen
  summaryFunction = defaultSummary
)

# 3) Random Forest trainen ---------------------------------------------

set.seed(42)
rf_mod <- train(
  x          = X_train,
  y          = y_train_fac,     
  method     = "rf",
  trControl  = ctrl,
  tuneLength = 5,
  metric     = "Accuracy"
)
# na uur draaien nog geen resultaat
#-- Ranger random forest
set.seed(42)
ranger_grid <- expand.grid(
  mtry            = floor(sqrt(ncol(X_train))),
  splitrule       = "gini",
  min.node.size   = c(5, 10)
)

rf_mod <- train(
  x          = X_train,
  y          = y_train_fac,
  method     = "ranger",
  trControl  = ctrl,
  tuneGrid   = ranger_grid,
  num.trees  = 200,           # minder bomen = sneller
  metric     = "Accuracy"
)

# 4) XGBoost trainen ---------------------------------------------------

set.seed(42)
xgb_mod <- train(
  x          = X_train,
  y          = y_train_fac,
  method     = "xgbTree",
  trControl  = ctrl,
  tuneLength = 5,          # zoekt automatisch over 5 combinaties van eta/max_depth/…
  metric     = "Accuracy"
)

# 5) Vergelijk de CV-resultaten ----------------------------------------

res <- resamples(list(RF = rf_mod, XGB = xgb_mod))
# gemiddelde accuracies en kappa per model:
summary(res)
# boxplots van de Accuracy‐scores
bwplot(res, metric = "Accuracy")

# 6) Test‐set evaluatie ------------------------------------------------

# 6a) voorspel
pred_rf  <- predict(rf_mod, X_test)
pred_xgb <- predict(xgb_mod, X_test)

# 6b) confusion matrices
cm_rf  <- confusionMatrix(pred_rf,  y_test_fac)
cm_xgb <- confusionMatrix(pred_xgb, y_test)

print(cm_rf)   # laat Accuracy, per-klasse sens/spec, etc. zien
print(cm_xgb)

# 7) Variabele belangrijkheid ------------------------------------------

# 7a) RF importance
imp_rf <- varImp(rf_mod, scale = FALSE)
plot(imp_rf, top = 20, main = "RF: Top 20 variable importance")

# 7b) XGB importance
imp_xgb <- varImp(xgb_mod, scale = FALSE)
plot(imp_xgb, top = 20, main = "XGB: Top 20 variable importance")



# num_classes <- length(levels_cat)
# params <- list(
#   objective   = "multi:softprob",
#   num_class   = num_classes,
#   eval_metric = "mlogloss"
# )
# 
# set.seed(123)
# xgb_model <- xgb.train(
#   params    = params,
#   data      = dtrain,
#   nrounds   = 100,
#   watchlist = list(train = dtrain),
#   verbose   = 0
# )
# 
# pred_probs  <- predict(xgb_model, dtest)
# pred_matrix <- matrix(pred_probs, ncol = num_classes, byrow = TRUE)
# pred_labels <- max.col(pred_matrix)
# pred_factor <- factor(levels_cat[pred_labels], levels = levels_cat)
# 
# confusionMatrix(pred_factor, factor(test$Categorie, levels = levels_cat))
# 







# haal eerst alle rijen zonder missing label eruit
train_clean <- train %>%
  filter(!is.na(Categorie))

# herdefinieer X_train en y_train op de schoongemaakte data
X_train <- train_clean %>%
  select(-doc_id, -Categorie) %>%
  as.matrix()

# let op: factor levels expliciet instellen
levs <- unique(df_labels$Categorie)
y_train <- as.numeric(factor(train_clean$Categorie, levels = levs)) - 1

# bouw dan pas de DMatrix
dtrain <- xgb.DMatrix(data = X_train, label = y_train)

test_clean <- test %>%
  filter(!is.na(Categorie))

X_test <- test_clean %>%
  select(-doc_id, -Categorie) %>%
  as.matrix()

y_test <- as.numeric(factor(test_clean$Categorie, levels = levs)) - 1

dtest <- xgb.DMatrix(data = X_test, label = y_test)

nrow(train_clean); length(y_train)
nrow(test_clean);  length(y_test)


# --- 3a) Combineer embeddings met labels ------------------------------

# Veronderstel: df_labels haalde je uit df_clean_MOR
df_labels <- df_clean_MOR %>%
  select(doc_id, Categorie)        # let op: exacte kolomnaam 'Categorie'

# Join met je document-embeddings
df_train <- df_doc_embeddings %>%
  inner_join(df_labels, by = "doc_id")

# --- 3b) Train-test split ------------------------------------------------

set.seed(123)
train_idx <- createDataPartition(df_train$Categorie, p = 0.8, list = FALSE)

train <- df_train[train_idx, ]
test  <- df_train[-train_idx, ]

# --- 3c) Maak DMatrix voor xgboost --------------------------------------

# 1) Feature-matrix (zonder doc_id en Categorie)
X_train <- train %>%
  select(-doc_id, -Categorie) %>%
  as.matrix()

X_test <- test %>%
  select(-doc_id, -Categorie) %>%
  as.matrix()

# 2) Labels omzetten naar 0-based integers
#    factor levels blijven behouden via as.numeric()
y_train <- as.numeric(factor(train$Categorie)) - 1
y_test  <- as.numeric(factor(test$Categorie, 
                             levels = levels(factor(train$Categorie)))) - 1
#    Zorg dat test-labels dezelfde factor-levels krijgen als train

# 3) Bouw de DMatrixen (let op: argument is 'label', niet 'Categorie')
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test,  label = y_test)

# --- 3d) Train het XGBoost-model -----------------------------------------

# Bepaal aantal unieke klassen uit je trainingsset
num_classes <- length(unique(train$Categorie))

# Stel de parameters in
params <- list(
  objective   = "multi:softprob",
  num_class   = num_classes,
  eval_metric = "mlogloss"          # bijv. multiclass logloss
)

# Train (100 rondes is een startpunt; je kunt hier cv of early_stopping gebruiken)
set.seed(123)
xgb_model <- xgb.train(
  params    = params,
  data      = dtrain,
  nrounds   = 100,
  watchlist = list(train = dtrain),
  verbose   = 0
)

# --- 3e) Evaluate ---------------------------------------------------------

# Predict geeft voor elke rij een vector van class-scores (length = num_classes)
pred_probs <- predict(xgb_model, dtest)

# Zet om naar matrix: elke rij is één voorbeeld, elk van de num_classes kolommen
pred_matrix <- matrix(pred_probs, ncol = num_classes, byrow = TRUE)

# Predicte het klasselabel met de hoogste kans
pred_labels <- max.col(pred_matrix)            # geeft 1-based indices
# Zet om naar factor met dezelfde levels als train$Categorie
pred_factor <- factor(
  levels(train$Categorie)[pred_labels],
  levels = levels(train$Categorie)
)

# Confusion matrix
confusionMatrix(pred_factor, factor(test$Categorie, levels = levels(train$Categorie)))