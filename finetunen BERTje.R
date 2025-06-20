##fintunen Bertje
# Stap 1: lees je RDS in en schrijf een JSONL voor Hugging Face
library(readr)
library(jsonlite)
library(reticulate)

# 1a. Lees je .rds-bestand (vervang pad/naam naar jouw bestand)
df <- readRDS("df_clean_MOR.rds")
df <- df %>% select(Omschrijving, Categorie)


df <- df %>%
  rename(text = Omschrijving, label = Categorie) %>%
  filter(!is.na(label) & label != "")



# 1b. Converteer elke rij naar een JSON-regel
json_lines <- apply(df, 1, function(row) {
  toJSON(
    list(text  = row[["text"]],
         label = row[["label"]]),
    auto_unbox = TRUE
  )
})

# 1c. Schrijf naar JSONL-bestand
writeLines(json_lines, "mor_labeled.jsonl")

# Stap 2: Splits de data in train en val (eerst maar 1000 gekozen)
lines <- readLines("mor_labeled.jsonl")
writeLines(lines[1:800], "train.jsonl")
writeLines(lines[(length(lines)-199):length(lines)], "val.jsonl")

# 3. Zet label ↔ ID mappings (voor training)
labels   <- sort(unique(df$label))
label2id <- setNames(seq_along(labels)-1, labels)
id2label <- setNames(labels, seq_along(labels)-1)



# 5. Laad en prepareer dataset
ds <- datasets$load_dataset(
  "json",
  data_files = dict(train="train.jsonl", validation="val.jsonl")
)
# cast label-kolom
ClassLabel <- datasets$ClassLabel
cls <- ClassLabel(names=labels)# 4. Koppel je Python‐omgeving en importeer libs
use_condaenv("bertje-env", required=TRUE)
datasets <- import("datasets")
transformers <- import("transformers")
evaluate    <- import("evaluate")
np          <- import("numpy")
ds <- ds$map(
  function(ex) dict(label=cls$str2int(ex[["label"]])),
  batched=TRUE
)

# 6. Tokenizer + tokenizatie
model_name <- "GroNLP/bert-base-dutch-cased"
tok        <- transformers$AutoTokenizer$from_pretrained(model_name)
tokenize_fn <- function(ex) {
  tok(
    ex[["text"]],
    padding     = "max_length",
    truncation  = TRUE,
    max_length  = as.integer(512)   # óf 512L
  )
}
ds <- ds$map(tokenize_fn, batched=TRUE)

# 7. Model initialiseren
model <- transformers$AutoModelForSequenceClassification$from_pretrained(
  model_name,
  num_labels = length(labels),
  id2label   = id2label,
  label2id   = label2id
)

# library(reticulate)
# use_condaenv("bertje-env", required = TRUE)  # je venv combineren werkt hetzelfde
# evaluate   <- import("evaluate")


# 8. Metrics
metric_acc <- evaluate$load("accuracy")
metric_f1  <- evaluate$load("f1")
compute_metrics <- function(p) {
  preds <- as.integer(np$argmax(p$predictions, axis=1))
  list(
    accuracy = metric_acc$compute(predictions=preds, references=p$label_ids)$accuracy,
    f1       = metric_f1$compute(predictions=preds, references=p$label_ids, average="weighted")$f1
  )
}




# 9. TrainingArguments & Trainer
TrainingArguments <- transformers$TrainingArguments
Trainer           <- transformers$Trainer
args <- TrainingArguments(
  output_dir = "./ft-bertje-mor",
  per_device_train_batch_size=8,
  per_device_eval_batch_size=8,
  evaluation_strategy="epoch",
  save_strategy="epoch",
  num_train_epochs=3,
  logging_dir="./logs",
  load_best_model_at_end=TRUE,
  metric_for_best_model="f1"
)

library(reticulate)
use_condaenv("bertje-env", required = TRUE)
transformers <- import("transformers")
transformers$`__version__`

# TrainingArguments <- transformers$TrainingArguments
# args <- TrainingArguments(
#   output_dir             = "./ft-bertje-mor",
#   evaluation_strategy    = "epoch",
#   save_strategy          = "epoch",
#   load_best_model_at_end = TRUE,
#   per_device_train_batch_size = 8L,
#   per_device_eval_batch_size  = 8L,
#   num_train_epochs       = 3L,
#   logging_dir            = "./logs"
# )
# trainer <- transformers$Trainer(
#   model            = model,
#   args             = args,
#   train_dataset    = ds$train,
#   eval_dataset     = ds$validation,
#   tokenizer        = tok,
#   compute_metrics  = compute_metrics
# )
# trainer$train()
# trainer$save_model("./ft-bertje-mor")

TrainingArguments <- transformers$TrainingArguments
Trainer           <- transformers$Trainer

args <- TrainingArguments(
  output_dir                   = "./ft-bertje-mor",
  per_device_train_batch_size  = 8L,
  per_device_eval_batch_size   = 8L,
  num_train_epochs             = 3L,
  logging_dir                  = "./logs"
)

# 1. Initialiseer de Trainer
trainer <- Trainer(
  model           = model,
  args            = args,
  train_dataset   = ds[["train"]],
  eval_dataset    = ds[["validation"]],
  compute_metrics = compute_metrics
)


# 2. Start de fine-tuning
trainer$train()

# 3. Sla je fine-tuned model op
trainer$save_model("./ft-bertje-mor")

