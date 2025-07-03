# Packages
library(tidyverse)
library(DBI)
library(odbc)

# Alvast paden definiëren
raw_data_output_path <- "raw_data/fixi_data.RDS"
raw_data_tokenized_output_path <- "raw_data/fixi_data_tokenized.RDS"
metadata_output_path <- "clean_data/fixi_metadata_clean.RDS"
tekst_output_path <- "clean_data/fixi_tekst_clean.RDS"
clean_data_output_path <- "clean_data/fixi_clean.RDS"

# Laad brondata in nieuw/bestaand 
# .csv bestand
path <- "c:/Users/jurriloo/OneDrive - Gemeente Katwijk/Bureaublad/fixi_2025_06_30.csv"
#cols <- c("id", "categoryName", "teamName", "source", "created",
#         "regionName", "description")
data <- read.csv(path, sep = ";", header = TRUE, stringsAsFactors = FALSE)

versie <- "bestaand"

if (versie == "bestaand") {
  data <- readRDS("raw_data/fixi_data.RDS")
} else if (versie == "nieuw") {
  sql_con <- dbConnect(odbc(),
                       driver = "ODBC Driver 17 for SQL Server",
                       server = "sql-dataplatform.database.windows.net",
                       database = "sqldb-dataplatform",
                       authentication = "ActiveDirectoryInteractive")  
  
  data <- dbGetQuery(sql_con, 
    "SELECT 
        [id]
        ,[categoryName]
        ,[teamName]
        ,[source]
        ,[created]
        ,[regionName]
        ,[description]
        
    FROM [FXI_E].[Issues]
        
    WHERE 1 = 1
    	AND created < '2025-07-01'")
  
  dbDisconnect(sql_con)
  
  # Ruwe data opslaan zodat niet steeds de verbinding hoeft te worden gemaakt bij testen
  saveRDS(data, raw_data_output_path)
}

# Bekijk welke categorieën er zijn
data |> 
  count(categoryName, sort = TRUE)

# Verwijder whitespace op het eind in de categorienamen en omschrijving
data$categoryName <- str_trim(data$categoryName)
data$description <- str_trim(data$description)

# Verwijder categorieën die minder dan 2% van alle meldingen bevatten
min_aantal <- nrow(data) * 0.02
categorieen <- data |> 
  count(categoryName) |> 
  filter(n >= min_aantal) |> 
  pull(categoryName)

# Verwijder irrelevante categorie (archief)
categorieen <- categorieen[categorieen != "Archief oude categorieen"]

data <- data |> 
  filter(categoryName %in% categorieen)

# Haal rijen eruit zonder tekst in de description kolom
data <- data |> 
  filter(description != "")

# Houd alles behalve de tekstuele data apart 
metadata <- data |> 
  select(-description)

# Roep de udpipe backend aan
cleanNLP::cnlp_init_udpipe("dutch", parser = "none")

# Tokenise de data
tekst_verwerkt <- data |> 
  cleanNLP::cnlp_annotate(text_name = "description", doc_name = "id")

View(tekst_verwerkt$token)

# Sla op 
saveRDS(tekst_verwerkt$token, raw_data_tokenized_output_path)
# Lees in
tekst_verwerkt <- readRDS(raw_data_tokenized_output_path)

# Alles naar lower case
tekst_verwerkt <- tekst_verwerkt |> 
  mutate(lemma = tolower(lemma))

# Filter bepaalde woordcategorieen eruit
# ADP (bindwoorden zoals voorzetsels), PUNCT (interpunctietekens), NUM (cijfers), SYM (tekens zoals € %), 
# AUX (hulpwerkwoorden zoals zijn, hebben), PRON (voornaamwoorden zoals ik, jij, hij), DET (lidwoorden en aanwijzende/ bezittelijke voornaamwoorden zoals de, dit)
# CCONJ (nevenschikkende voegwoorden zoals en, maar) 
tekst_verwerkt <- tekst_verwerkt |> 
  filter(!upos %in% c("ADP", "PUNCT", "NUM", "SYM", "AUX", "PRON", "DET", "CCONJ", "X"))

# Verwijder ook de stopwoorden 
stop_NL <- read.csv("https://raw.githubusercontent.com/stopwords-iso/stopwords-nl/master/stopwords-nl.txt", col.names = "stopwoord")
tekst_verwerkt <- tekst_verwerkt |> 
  filter(!lemma %in% stop_NL$stopwoord)

# Tel woorden
print(tekst_verwerkt |> 
  count(lemma, sort = TRUE), n = 50)

# Update de stopwoordenlijst
stop_NL <- stop_NL |> 
  rbind("staan", "graag", "liggen", "gaan", "komen", "zitten", "zien", "week", "melding", "groot", "lopen", "gemeente", "plaatsen", "kijken",
          "geven", "laten", "gebeuren", "blijven", "waardoor", "meerdere")

tekst_verwerkt <- tekst_verwerkt |> 
  filter(!lemma %in% stop_NL$stopwoord)

saveRDS(metadata, metadata_output_path)
saveRDS(tekst_verwerkt, tekst_output_path)
saveRDS(data, clean_data_output_path)