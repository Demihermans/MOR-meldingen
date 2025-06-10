# Laad bronbestand in
path <- "C:/Users/jurvd/OneDrive/Bureaublad/fixi_meldingen_20250521.csv"
cols <- c("teamName", "subCatName", "source", "regionName", "address", "addressDetails",
         "categoryName", "created", "closed", "description", "latitude", "longitude", "status")
description_col <- "description"
metadata_output_path <- "clean_data/fixi_metadata_clean.RDS"
tekst_output_path <- "clean_data/fixi_tekst_clean.RDS"

data <- read.csv(path, sep = ";", header = FALSE, stringsAsFactors = FALSE,
                col.names = cols
                      )

# Bekijk welke categorieën er zijn
data |> 
  count(categoryName, sort = TRUE)

# Verwijder whitespace op het eind in de categorienamen
data$categoryName <- str_trim(data$categoryName)

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

# Houd alles behalve de tekstuele data apart en wijs rijnummer toe (voor koppeling op doc_id)
metadata <- data |> 
  select(-description) |> 
  mutate(doc_id = row_number())

# Roep de udpipe backend aan
cleanNLP::cnlp_init_udpipe("dutch", parser = "none")

# Tokenise de data
tekst_verwerkt <- data |> 
  cleanNLP::cnlp_annotate(text_name = description_col)

View(tekst_verwerkt$token)

# Filter bepaalde woordcategorieen eruit
# ADP (bindwoorden zoals voorzetsels), PUNCT (interpunctietekens), NUM (cijfers), SYM (tekens zoals € %), 
# AUX (hulpwerkwoorden zoals zijn, hebben), PRON (voornaamwoorden zoals ik, jij, hij), DET (lidwoorden en aanwijzende/ bezittelijke voornaamwoorden zoals de, dit)
# CCONJ (nevenschikkende voegwoorden zoals en, maar) 
tekst_verwerkt <- tekst_verwerkt$token |> 
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
  rbind("staan", "graag", "liggen", "gaan", "komen", "zitten", "zien", "groot", "lopen", "gemeente", "plaatsen", "kijken",
          "geven", "laten", "gebeuren", "blijven", "waardoor", "meerdere")

tekst_verwerkt <- tekst_verwerkt |> 
  filter(!lemma %in% stop_NL$stopwoord)

saveRDS(metadata, metadata_output_path)
saveRDS(tekst_verwerkt, tekst_output_path)
