library(tidyverse)
library(ggplot2)

data <- readRDS("clean_data/fixi_clean.RDS")

# Maak datum type van created
data <- data |> 
  mutate(created = as.Date(created, format = "%Y-%m-%d"))

# Voeg kolommen jaar, kwartaal, maand toe aan data
data <- data |> 
  mutate(
    jaar = format(created, "%Y"),
    kwartaal = quarters(created),
    maand = format(created, '%m'),
    jaarkwartaal = paste(jaar, kwartaal, sep = " "),
    jaarmaand = paste(jaar, maand, sep = " ")
  )

data <- data |> 
  filter(jaar > 2018)

# Plot het aantal meldingen per jaar
ggplot(data, aes(x = jaar)) +
  geom_bar() +
  labs(title = "Aantal meldingen per jaar", x = "Jaar", y = "Aantal meldingen") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")

# Aantal meldingen per jaar en kwartaal
ggplot(data, aes(x = jaarkwartaal)) +
  geom_bar() +
  labs(title = "Aantal meldingen per kwartaal", x = "Jaar - kwartaal", y = "Aantal meldingen") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")

# Aantal meldingen per jaar en maand
ggplot(data, aes(x = jaarmaand)) +
  geom_bar() +
  labs(title = "Aantal meldingen per maand", x = "Jaar - maand", y = "Aantal meldingen") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))

# Aantal meldingen per categorie
ggplot(data, aes(x = fct_rev(fct_infreq(categoryName)))) +
  geom_bar() +
  labs(title = "Aantal meldingen per categorie", x = "Categorie", y = "Aantal meldingen") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none") +
  coord_flip()
