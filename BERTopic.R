library(reticulate)
library(tidyverse)

# Specificeer pad naar de virtuele Python omgeving
path <- "C:/Users/jurvd/Documents/DSBA/MOR-meldingen/bertopic_env"

# Maak virtuele omgeving aan (alleen eerste keer)
# virtualenv_create(envname= path)

# Activeer virtuele omgeving
use_virtualenv(path, required = TRUE)

# Installeer Python packages in de virtuele omgeving (alleen eerste keer)
# py_install(packages = c("bertopic", "umap-learn", "hdbscan", "pandas", "sentence_transformers", "hf_xet"), envname = path)

# Laad data
df <- readRDS("raw_data/fixi_data.rds")

# Alleen beschrijving nodig voor unsupervised learning
df <- df |> 
  select(description) |> 
  slice(1:1000)  # Beperk tot 1000 rijen voor snellere verwerking

# Initialiseer BERTopic en SentenceTransformer vanuit Python
BERTopic <- import("bertopic")$BERTopic
SentenceTransformer <- import("sentence_transformers")$SentenceTransformer
# HDBSCAN <- import("hdbscan")$HDBSCAN

# Laad een Nederlands embedding model vanuit SentenceTransformer
embedding_model <- SentenceTransformer("distiluse-base-multilingual-cased-v1") # "paraphrase-multilingual-MiniLM-L12-v2")

# Maak een HDBSCAN model met een minimum cluster grootte van 50 (R gebruikt doubles, dus forceer naar integer)
# hdbscan_model <- HDBSCAN(min_cluster_size = as.integer(25), min_samples = as.integer(5), metric = "euclidean", cluster_selection_method = "eom")

# Maak het topic model aan
topic_model <- BERTopic(
  embedding_model = embedding_model,
  #hdbscan_model = hdbscan_model,
  language = "nl",
  calculate_probabilities = TRUE,
  verbose = TRUE)

# Fit het topic model op de data
topics <- topic_model$fit_transform(df$description)
  
# Laat de topics zien
topic_info <- topic_model$get_topic_info()

topic_info$Representative_Docs[1]
