### Supervised model training ###
library(dplyr)
library(caret)
library(ranger)
library(xgboost)
library(Matrix)

# --- Data klaarmaken ----------------------------------------------------

# Labels uit gelabelde df_clean_MOR halen
df_labels <- df_clean_MOR %>% select(doc_id, Categorie)

# Join embeddings met labels en lege labels eruit filteren
df_data <- df_doc_embeddings %>%
  inner_join(df_labels, by = "doc_id") %>%
  filter(!is.na(Categorie)) %>%
  mutate(
    Categorie_clean = make.names(Categorie),
    Categorie_clean = factor(Categorie_clean,
                             levels = unique(make.names(Categorie)))
  )

# Train/test-split
set.seed(123)
train_idx <- createDataPartition(df_data$Categorie_clean, p = .8, list = FALSE)
train      <- df_data[train_idx, ]
test       <- df_data[-train_idx, ]

# Features; alle embedding kolommen
X_train      <- train %>% select(-doc_id, -Categorie, -Categorie_clean) %>% as.matrix()
X_test       <- test  %>% select(-doc_id, -Categorie, -Categorie_clean) %>% as.matrix()

# Labels; factor en numeriek
y_train_fac  <- train$Categorie_clean
y_test_fac   <- test$Categorie_clean

# Voor xgboost: 0-based numeric labels
levels_cat <- levels(y_train_fac)
y_train <- as.numeric(y_train_fac) - 1
y_test  <- as.numeric(y_test_fac)  - 1


# --- 1) Random Forest (ranger) -----------------------------------------

ctrl <- trainControl(
  method      = "cv",
  number      = 3,            # 3-fold CV voor snelle prototype, kan ik nog op 5 of 10 zetten.
  classProbs  = TRUE,
  summaryFunction = defaultSummary
)

set.seed(42)
ranger_grid <- expand.grid(
  mtry          = floor(sqrt(ncol(X_train))),
  splitrule     = "gini",
  min.node.size = c(5, 10)
)

rf_mod <- train(
  x         = X_train,
  y         = y_train_fac,
  method    = "ranger",
  trControl = ctrl,
  tuneGrid  = ranger_grid,
  num.trees = 200, #duurt 20 minuten deze instellingen, kan ook meer trees proberen met obb 
  metric    = "Accuracy"
)

print(rf_mod)

#--------- Voorspellen en evalueren ---------------------
#  Voorspel harde labels op je test‐set
pred_rf <- predict(rf_mod, X_test)

# 2) Optioneel: voorspel ook klasse-probabilities
# probs_rf <- predict(rf_mod, X_test, type = "prob")

# Confusion Matrix
cm_rf <- confusionMatrix(pred_rf, y_test_fac)
print(cm_rf)

# --- 2) XGBoost via xgb.cv + xgb.train -------------------------------

# a) Zet data in DMatrix
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test,  label = y_test)

# b) Basis-parameters voor snelle training
params <- list(
  objective        = "multi:softprob",
  num_class        = length(levels_cat),
  eta              = 0.1,
  max_depth        = 6,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  eval_metric      = "mlogloss",
  tree_method      = "hist",             # gebruik de histogram‐split
  nthread          = parallel::detectCores() - 1
)

# c) 3-fold CV met early stopping (max 500 ronden, stopt na 20 zonder verbetering)
set.seed(42)
cv_res <- xgb.cv(
  params                = params,
  data                  = dtrain,
  nrounds               = 500,
  nfold                 = 3,
  early_stopping_rounds = 20,
  verbose               = 1,
  maximize              = FALSE
)

best_nrounds <- cv_res$best_iteration
cat("Beste aantal ronden voor XGBoost:", best_nrounds, "\n")

# d) Train het finale XGBoost-model met dat aantal ronden
set.seed(42)
xgb_final <- xgb.train(
  params  = params,
  data    = dtrain,
  nrounds = best_nrounds,
  verbose = 1
)
 #dit duurt ongeveer 15 minuten. 

# ---  Voorspellen en evalueren ---------------------------------------

# Predict class-probabilities en zet om naar harde labels
pred_probs <- predict(xgb_final, dtest)
pred_mat   <- matrix(pred_probs, ncol = length(levels_cat), byrow = TRUE)
pred_idx   <- max.col(pred_mat)        # 1-based indices
pred_factor<- factor(levels_cat[pred_idx], levels = levels_cat)

# Confusion matrix
cm_xgb <- confusionMatrix(pred_factor, y_test_fac)
print(cm_xgb)

# ---Variabele-importanties -----------------------------------------

# # RF importance
# imp_rf <- varImp(rf_mod, scale = FALSE)
# plot(imp_rf, top = 20, main = "Ranger RF: top 20 variable importance")
# 
# # XGB importance
# imp_xgb <- xgb.importance(model = xgb_final)
# xgb.plot.importance(imp_xgb[1:20, ], rel_to_first = TRUE, main = "XGB: top 20 importance")




