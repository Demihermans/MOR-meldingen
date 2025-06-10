# Packages
library(tidyverse)

# Laad bronbestand in
path <- "C:/Users/jurvd/OneDrive/Bureaublad/fixi_meldingen_20250521.csv"
cols <- c("teamName", "subCatName", "source", "regionName", "address", "addressDetails",
          "categoryName", "created", "closed", "description", "latitude", "longitude", "status")

data <- read.csv(path, sep = ";", header = FALSE, stringsAsFactors = FALSE, col.names = cols)

# Aantal lege rijen van elke kolom
empty_rows <- sapply(data, function(x) sum(x == ""))
empty_rows

# Verwijder subCatName en addressDetails
data <- data |> 
  select(-subCatName, -addressDetails, -closed)
