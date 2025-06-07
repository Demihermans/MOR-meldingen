# Topic modelling (zonder naar labels/ categorie te kijken)


library(tm)
library(topicmodels)
library(broom)

#Document Term matrix ("dtm") al aangemaakt in script text analysis

#LDA (Latent Dirichlet Allocation)
k <- 7  # kies het aantal topics

#LDA met Gibbs sampling (probabilistische schatting)
lda_gibbs <- LDA(dtm, k = k, method = "Gibbs",
                 control = list(seed = 1234, burnin = 2000, iter = 5000, thin = 100))

# Of LDA met Variational Bayes

lda_vb <- LDA(
  dtm,
  k = k,
  method = "VEM",
  control = list(
    seed           = 1234,
    estimate.beta  = TRUE    # laat het woord‐per‐topic (β) mee schatten
    # optioneel kun je hier nog toevoegen:
    #    em.max         = 1000,   # max. aantal EM-iteraties (standaard is 200)
    #    var.max        = 500,    # max. aantal iteraties in de variational step
    #    verbose        = 1       # zet op >0 om tijdens fit berichten te zien
  )
)

# Top 10 woorden per topic uit Gibbs-run
tidy(lda_gibbs, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  ggplot(aes(reorder_within(term, beta, topic), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  labs(x = NULL, y = "P(term | topic)",
       title = "LDA (Gibbs): top 10 woorden per topic")

# op de x=as zie je hoe groot de kans is dat een willekeurige keer dat woord voorkomt, het woord bij dit onderwerp hoort
# op de y-as de woorden zelf die gerangschikt zijn naar de hoogste kans bovenaaan.

# topic 1; kapotte objecten in de openbare ruimte
# topic 2: verkeersveiligheid/ staat van wegdek
# Topic 3: parkeren (mensen zetten er waarschijnlijk bij hoe lang een auto er al staat (week/dag/tijd))
# topic 4: woorden die behoren tot communicatietaal als vrijdelijke groet, onderwerp, naam is etc. Geen concreet onderwerp over de melding
# topic 5: ook niet een duidelijk onderwerp, meer algemene zinnen over woon situaties
# topic 5: lijkt over gebrekkige bestrating of wateroverlast te gaan.
# topic 6: groen onderhoud/ bomen snoeien/ onkruid maar ook over ratten
# topic 7: zwerfvuil, afval dumpingen, (ondergrondse) containers
# Topic 8: algemene taal, niet echt specifiek op een onderwerp. Hond lijkt nog meest informatief

# Top 10 woorden per topic (hoogste β = P(term|topic)) met Variational Bayes
tidy(lda_vb, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  ggplot(aes(reorder_within(term, beta, topic), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  labs(x = NULL, y = "P(term | topic)",
       title = "LDA (VEM): top 10 woorden per topic")

#conclusie; Deze benadering geeft een wat vervuilder beeld dan de Gibbs benadering. Je ziet meer verschillende woorden per topic en meer algemene of lege tokens.
# topic 2 kan bijvoorbeeld over afval gaan maar ook over groen (boom). De woorden container, zak en afval staan weer in andere topics (7 en 8)
# Om deze reden gekozen om verder te gaan met de Gibbs run

# Topic per document
#  Haal de gamma-waarden op (elk doc_id, elk topic, met γ)
topics_gamma <- tidy(lda_gibbs, matrix = "gamma") #gamma is de score hoe sterk dat topic in dat document aanwezig is (kans dat een willekeurig woord uit dat document bij specifieke onderwerp hoort)
# topics_gamma heeft nu kolommen: document, topic, gamma

# Kies per document het topic met de hoogste gamma
top_topic_per_doc <- topics_gamma %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%   # pakt voor elk document de rij met hoogste gamma
  ungroup() %>%
  rename(
    doc_id    = document,       # hernoem kolom 'document' naar 'doc_id'
    lda_topic = topic           # hernoem kolom 'topic' naar 'lda_topic'
  ) %>%
  select(doc_id, lda_topic)     # we hebben enkel doc_id + gekozen topic nodig

##hoe vaak wordt elk topic als hoogste kans gekozen?
topic_counts <- top_topic_per_doc %>%
  count(lda_topic, name = "count") %>%
  arrange(desc(count))